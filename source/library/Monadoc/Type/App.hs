{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Monadoc.Type.App where

import qualified Control.Monad.Base as Base
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans as Trans
import qualified Control.Monad.Trans.Control as Control
import qualified Monadoc.Class.MonadHttp as MonadHttp
import qualified Monadoc.Class.MonadSay as MonadSay
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Class.MonadTime as MonadTime
import qualified Monadoc.Extra.ResourcePool as Pool
import qualified Monadoc.Type.Context as Context
import qualified Say

type App = AppT IO

runApp :: Context.Context -> App a -> IO a
runApp context = flip Reader.runReaderT context . runAppT

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

instance Control.MonadBaseControl IO m => MonadHttp.MonadHttp (AppT m) where
  withManager callback = do
    context <- Reader.ask
    callback $ Context.manager context

instance Base.MonadBase IO m => MonadSay.MonadSay (AppT m) where
  hSay h = Base.liftBase . Say.hSay h

instance Control.MonadBaseControl IO m => MonadSql.MonadSql (AppT m) where
  withConnection callback = do
    context <- Reader.ask
    Pool.withResourceLifted (Context.pool context) callback

instance Base.MonadBase IO m => MonadTime.MonadTime (AppT m) where
  getCurrentTime = Base.liftBase MonadTime.getCurrentTime
  getMonotonicTime = Base.liftBase MonadTime.getMonotonicTime
