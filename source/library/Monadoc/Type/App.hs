{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Monadoc.Type.App where

import qualified Control.Monad as Monad
import qualified Control.Monad.Base as Base
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans as Trans
import qualified Control.Monad.Trans.Control as Control
import qualified Monadoc.Class.MonadFile as MonadFile
import qualified Monadoc.Class.MonadHttp as MonadHttp
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSay as MonadSay
import qualified Monadoc.Class.MonadSleep as MonadSleep
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Class.MonadTime as MonadTime
import qualified Monadoc.Extra.ResourcePool as Pool
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Client as Client
import qualified Say
import qualified System.Directory as Directory

type App = AppT IO

newtype AppT m a = AppT
  { runAppT :: Reader.ReaderT Context.Context m a
  }
  deriving
    ( Applicative,
      Functor,
      Monad,
      Base.MonadBase b,
      Control.MonadBaseControl b,
      Exception.MonadCatch,
      Exception.MonadMask,
      Reader.MonadReader Context.Context,
      Exception.MonadThrow,
      Trans.MonadTrans
    )

instance Base.MonadBase IO m => MonadFile.MonadFile (AppT m) where
  getModificationTime = Base.liftBase . Directory.getModificationTime

instance Control.MonadBaseControl IO m => MonadHttp.MonadHttp (AppT m) where
  withResponse request f = do
    context <- Reader.ask
    Control.control $ \runInBase ->
      Client.withResponse request (Context.manager context) $ runInBase . f

instance Base.MonadBase IO m => MonadLog.MonadLog (AppT m) where
  log severity message = do
    context <- Reader.ask
    Monad.when (severity >= Config.severity (Context.config context))
      . Base.liftBase
      $ MonadLog.log severity message

instance Base.MonadBase IO m => MonadSay.MonadSay (AppT m) where
  hSay h = Base.liftBase . Say.hSay h

instance Base.MonadBase IO m => MonadSleep.MonadSleep (AppT m) where
  sleep = Base.liftBase . MonadSleep.sleep

instance (Monad m, Control.MonadBaseControl IO m) => MonadSql.MonadSql (AppT m) where
  withConnection callback = do
    context <- Reader.ask
    Pool.withResourceLifted (Context.pool context) callback

instance Base.MonadBase IO m => MonadTime.MonadTime (AppT m) where
  getCurrentTime = Base.liftBase MonadTime.getCurrentTime
  getMonotonicTime = Base.liftBase MonadTime.getMonotonicTime
