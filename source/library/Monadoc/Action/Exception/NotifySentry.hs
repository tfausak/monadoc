{-# LANGUAGE FlexibleContexts #-}

module Monadoc.Action.Exception.NotifySentry where

import qualified Control.Monad.Base as Base
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Data.Text as Text
import qualified Monadoc.Class.MonadHttp as MonadHttp
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Patrol.Client as Patrol
import qualified Patrol.Type.Event as Patrol.Event
import qualified Patrol.Type.Exception as Patrol.Exception

run ::
  ( Base.MonadBase IO m,
    MonadHttp.MonadHttp m,
    MonadLog.MonadLog m,
    Reader.MonadReader Context.Context m
  ) =>
  Exception.SomeException ->
  m ()
run exception = do
  context <- Reader.ask
  case Config.dsn $ Context.config context of
    Nothing -> pure ()
    Just dsn -> MonadHttp.withManager $ \manager -> do
      event <- Base.liftBase Patrol.Event.new
      eventId <-
        Base.liftBase $
          Patrol.store
            manager
            dsn
            event
              { Patrol.Event.exception =
                  Just
                    [ Patrol.Exception.fromSomeException exception
                    ],
                Patrol.Event.release = Text.pack <$> Context.sha context
              }
      MonadLog.warn . Text.pack $ show eventId
