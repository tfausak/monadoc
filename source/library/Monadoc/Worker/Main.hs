{-# LANGUAGE FlexibleContexts #-}

module Monadoc.Worker.Main where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans.Control as Control
import qualified Monadoc.Action.CronEntry.Enqueue as CronEntry.Enqueue
import qualified Monadoc.Action.CronEntry.Prune as CronEntry.Prune
import qualified Monadoc.Action.CronEntry.Upsert as CronEntry.Upsert
import qualified Monadoc.Action.Job.Acquire as Job.Acquire
import qualified Monadoc.Action.Job.Perform as Job.Perform
import qualified Monadoc.Action.Job.Release as Job.Release
import qualified Monadoc.Class.MonadHttp as MonadHttp
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSleep as MonadSleep
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Constant.CronEntry as CronEntry
import qualified Monadoc.Middleware.HandleExceptions as HandleExceptions
import qualified Monadoc.Type.Context as Context

worker ::
  ( Control.MonadBaseControl IO m,
    MonadHttp.MonadHttp m,
    MonadLog.MonadLog m,
    Exception.MonadMask m,
    Reader.MonadReader Context.Context m,
    MonadSleep.MonadSleep m,
    MonadSql.MonadSql m
  ) =>
  m ()
worker = do
  mapM_ CronEntry.Upsert.run CronEntry.all
  CronEntry.Prune.run

  Monad.forever $ do
    CronEntry.Enqueue.run
    Exception.handleAll HandleExceptions.onException $ do
      ((), ()) <- Exception.generalBracket Job.Acquire.run Job.Release.run Job.Perform.run
      pure ()
