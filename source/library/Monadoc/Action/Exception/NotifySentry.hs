{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.Exception.NotifySentry where

import qualified Control.Monad as Monad
import qualified Control.Monad.Base as Base
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Data.Text as Text
import qualified Monadoc.Class.MonadHttp as MonadHttp
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Exception.MethodNotAllowed as MethodNotAllowed
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Exception.UnknownRoute as UnknownRoute
import qualified Monadoc.Extra.Exception as Exception
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Network.Wai.Handler.Warp as Warp
import qualified Patrol
import qualified Patrol.Client as Patrol
import qualified Patrol.Type.Event as Patrol.Event
import qualified Patrol.Type.Exception as Patrol.Exception
import qualified Patrol.Type.StackTrace as Patrol.StackTrace

run ::
  ( Base.MonadBase IO m,
    MonadHttp.MonadHttp m,
    MonadLog.MonadLog m,
    Reader.MonadReader Context.Context m
  ) =>
  (Patrol.Event -> Patrol.Event) ->
  Exception.SomeException ->
  m ()
run f exception = Monad.when (shouldNotify exception) $ do
  context <- Reader.ask
  case Config.dsn $ Context.config context of
    Nothing -> pure ()
    Just dsn -> MonadHttp.withManager $ \manager -> do
      event <- Base.liftBase Patrol.Event.new
      eventId <-
        Base.liftBase
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
                Patrol.Event.release = Text.pack <$> Context.sha context
              }
      MonadLog.warn . Text.pack $ show eventId

shouldNotify :: Exception.SomeException -> Bool
shouldNotify e =
  Warp.defaultShouldDisplayException e
    && Exception.isSync e
    && Exception.isNotType @UnknownRoute.UnknownRoute e
    && Exception.isNotType @MethodNotAllowed.MethodNotAllowed e
