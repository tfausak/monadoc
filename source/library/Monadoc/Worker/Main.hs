{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Worker.Main where

import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Exception (AsyncException, SomeAsyncException)
import qualified Control.Monad as Monad
import qualified Control.Monad.Base as Base
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans.Control as Control
import qualified Monadoc.Action.CronEntry.Enqueue as CronEntry.Enqueue
import qualified Monadoc.Action.CronEntry.Prune as CronEntry.Prune
import qualified Monadoc.Action.CronEntry.Upsert as CronEntry.Upsert
import qualified Monadoc.Action.Exception.Log as Exception.Log
import qualified Monadoc.Action.Exception.NotifySentry as Exception.NotifySentry
import qualified Monadoc.Action.Job.Acquire as Job.Acquire
import qualified Monadoc.Action.Job.Perform as Job.Perform
import qualified Monadoc.Action.Job.Release as Job.Release
import qualified Monadoc.Class.MonadHttp as MonadHttp
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSleep as MonadSleep
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Constant.CronEntry as CronEntry
import qualified Monadoc.Extra.Exception as Exception
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
    Exception.handleJust rejectAsync onException
      . Monad.void
      $ Exception.generalBracket Job.Acquire.run Job.Release.run Job.Perform.run

rejectAsync :: Exception.SomeException -> Maybe Exception.SomeException
rejectAsync e = do
  Monad.guard . not $ Exception.isType @Async.AsyncCancelled e
  Monad.guard . not $ Exception.isType @Exception.AsyncException e
  Monad.guard . not $ Exception.isType @Exception.SomeAsyncException e
  pure e

onException ::
  ( Base.MonadBase IO m,
    MonadHttp.MonadHttp m,
    MonadLog.MonadLog m,
    Reader.MonadReader Context.Context m
  ) =>
  Exception.SomeException ->
  m ()
onException exception = do
  Exception.Log.run exception
  Exception.NotifySentry.run exception
