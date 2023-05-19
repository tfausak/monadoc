module Monadoc.Action.Exception.NotifySentry where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Aeson as Aeson
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Version as Version
import qualified GHC.Stack as Stack
import qualified Monadoc.Action.App.Log as App.Log
import qualified Monadoc.Exception.Found as Found
import qualified Monadoc.Exception.MethodNotAllowed as MethodNotAllowed
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Exception.UnknownRoute as UnknownRoute
import qualified Monadoc.Extra.Exception as Exception
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Network.Wai.Handler.Warp as Warp
import qualified Paths_monadoc as Monadoc
import qualified Patrol
import qualified Patrol.Client as Patrol
import qualified Patrol.Type.Event as Patrol.Event
import qualified Patrol.Type.EventId as Patrol.EventId
import qualified Patrol.Type.Exception as Patrol.Exception
import qualified Patrol.Type.Exceptions as Patrol.Exceptions
import qualified Patrol.Type.Frame as Patrol.Frame
import qualified Patrol.Type.Request as Patrol.Request
import qualified Patrol.Type.Response as Patrol.Response
import qualified Patrol.Type.Stacktrace as Patrol.Stacktrace
import qualified System.Environment as Environment

run ::
  (Patrol.Event -> Patrol.Event) ->
  Exception.SomeException ->
  App.App ()
run f exception = Monad.when (shouldNotify exception) $ do
  context <- Reader.ask
  Monad.forM_ (Config.dsn $ Context.config context) $ \dsn -> do
    let manager = Context.manager context
    event <- IO.liftIO Patrol.Event.new
    environment <- IO.liftIO Environment.getEnvironment
    response <-
      IO.liftIO
        . Patrol.store
          manager
          dsn
        $ f
          event
            { Patrol.Event.exception =
                Just
                  Patrol.Exceptions.empty
                    { Patrol.Exceptions.values =
                        [ case Exception.fromException exception of
                            Nothing -> Patrol.Exception.fromSomeException exception
                            Just (Traced.Traced e s) ->
                              (Patrol.Exception.fromSomeException e)
                                { Patrol.Exception.stacktrace = Just $ makeStacktrace s
                                }
                        ]
                    },
              Patrol.Event.release = Config.sha $ Context.config context,
              Patrol.Event.request =
                Just
                  Patrol.Request.empty
                    { Patrol.Request.env =
                        Map.fromList $
                          fmap (Bifunctor.bimap Text.pack Aeson.toJSON) environment
                    }
            }
    App.Log.warn $ "Sentry event ID: " <> Patrol.EventId.intoText (Patrol.Response.id response)

shouldNotify :: Exception.SomeException -> Bool
shouldNotify e =
  Warp.defaultShouldDisplayException e
    && Exception.isSync e
    && Exception.isNotType @Found.Found e
    && Exception.isNotType @MethodNotAllowed.MethodNotAllowed e
    && Exception.isNotType @NotFound.NotFound e
    && Exception.isNotType @UnknownRoute.UnknownRoute e

makeStacktrace :: Stack.CallStack -> Patrol.Stacktrace.Stacktrace
makeStacktrace callStack =
  let stacktrace = Patrol.Stacktrace.fromCallStack callStack
      f x =
        x
          { Patrol.Frame.contextLine = Text.pack "... " <> Patrol.Frame.function x <> Text.pack " ...",
            Patrol.Frame.inApp = Just $ Patrol.Frame.package x == Text.pack ("monadoc-" <> Version.showVersion Monadoc.version <> "-inplace")
          }
   in stacktrace {Patrol.Stacktrace.frames = f <$> Patrol.Stacktrace.frames stacktrace}
