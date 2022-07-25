module Monadoc.Action.Exception.NotifySentry where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Monadoc.Action.App.Log as App.Log
import qualified Monadoc.Exception.Found as Found
import qualified Monadoc.Exception.MethodNotAllowed as MethodNotAllowed
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Exception.UnknownRoute as UnknownRoute
import qualified Monadoc.Extra.Exception as Exception
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Network.Wai.Handler.Warp as Warp
import qualified Patrol
import qualified Patrol.Client as Patrol
import qualified Patrol.Type.Event as Patrol.Event
import qualified Patrol.Type.Exception as Patrol.Exception
import qualified Patrol.Type.Request as Patrol.Request
import qualified Patrol.Type.StackTrace as Patrol.StackTrace
import qualified System.Environment as Environment

run ::
  (Patrol.Event -> Patrol.Event) ->
  Exception.SomeException ->
  App.App ()
run f exception = Monad.when (shouldNotify exception) $ do
  context <- Reader.ask
  case Config.dsn $ Context.config context of
    Nothing -> pure ()
    Just dsn -> do
      let manager = Context.manager context
      event <- IO.liftIO Patrol.Event.new
      environment <- IO.liftIO Environment.getEnvironment
      eventId <-
        IO.liftIO
          . Patrol.store
            manager
            dsn
          $ f
            event
              { Patrol.Event.exception =
                  Just
                    [ case Exception.fromException exception of
                        Nothing -> Patrol.Exception.fromSomeException exception
                        Just (Traced.Traced e s) ->
                          (Patrol.Exception.fromSomeException e)
                            { Patrol.Exception.stackTrace = Patrol.StackTrace.fromCallStack s
                            }
                    ],
                Patrol.Event.release = Text.pack <$> Context.sha context,
                Patrol.Event.request =
                  Just
                    emptyRequest
                      { Patrol.Request.env =
                          Just
                            . Map.fromList
                            $ fmap (Bifunctor.bimap Text.pack Text.pack) environment
                      }
              }
      App.Log.warn . Text.pack $ show eventId

emptyRequest :: Patrol.Request.Request
emptyRequest =
  Patrol.Request.Request
    { Patrol.Request.cookies = Nothing,
      Patrol.Request.data_ = Nothing,
      Patrol.Request.env = Nothing,
      Patrol.Request.headers = Nothing,
      Patrol.Request.method = Nothing,
      Patrol.Request.queryString = Nothing,
      Patrol.Request.url = Nothing
    }

shouldNotify :: Exception.SomeException -> Bool
shouldNotify e =
  Warp.defaultShouldDisplayException e
    && Exception.isSync e
    && Exception.isNotType @UnknownRoute.UnknownRoute e
    && Exception.isNotType @MethodNotAllowed.MethodNotAllowed e
    && Exception.isNotType @Found.Found e
