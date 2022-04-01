{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Worker.Main where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans.Control as Control
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Monadoc.Action.Database.Vacuum as Database.Vacuum
import qualified Monadoc.Action.HackageIndex.Process as HackageIndex.Process
import qualified Monadoc.Action.HackageIndex.Upsert as HackageIndex.Upsert
import qualified Monadoc.Action.Job.Enqueue as Job.Enqueue
import qualified Monadoc.Class.MonadHttp as MonadHttp
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSleep as MonadSleep
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Class.MonadTime as MonadTime
import qualified Monadoc.Model.Job as Job
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Status as Status
import qualified Monadoc.Type.Task as Task
import qualified Witch

worker :: (Control.MonadBaseControl IO m, MonadHttp.MonadHttp m, MonadLog.MonadLog m, Exception.MonadMask m, Reader.MonadReader Context.Context m, MonadSleep.MonadSleep m, MonadSql.MonadSql m) => m ()
worker = do
  Monad.void $ Job.Enqueue.run Task.Vacuum
  Monad.void $ Job.Enqueue.run Task.UpsertHackageIndex
  Monad.void $ Job.Enqueue.run Task.ProcessHackageIndex

  Monad.forever $ Exception.generalBracket acquireJob releaseJob runJob

acquireJob :: (MonadSql.MonadSql m, MonadTime.MonadTime m) => m (Maybe Job.Model)
acquireJob = do
  rows <-
    MonadSql.query
      "select * from job where status = ? order by createdAt asc limit 1"
      [Status.Queued]
  case rows of
    [] -> pure Nothing
    job : _ -> do
      now <- MonadTime.getCurrentTime
      MonadSql.execute
        "update job set startedAt = ?, status = ? where key = ?"
        (now, Status.Locked, Model.key job)
      pure $ Just job

releaseJob :: (MonadSql.MonadSql m, MonadTime.MonadTime m) => Maybe Job.Model -> Exception.ExitCase () -> m ()
releaseJob maybeJob exitCase = case maybeJob of
  Nothing -> pure ()
  Just job -> do
    now <- MonadTime.getCurrentTime
    let status = case exitCase of
          Exception.ExitCaseSuccess _ -> Status.Passed
          Exception.ExitCaseException _ -> Status.Failed
          Exception.ExitCaseAbort -> Status.Failed
    MonadSql.execute
      "update job set finishedAt = ?, status = ? where key = ?"
      (now, status, Model.key job)

runJob ::
  ( Control.MonadBaseControl IO m,
    MonadHttp.MonadHttp m,
    MonadLog.MonadLog m,
    Exception.MonadMask m,
    Reader.MonadReader Context.Context m,
    MonadSleep.MonadSleep m,
    MonadSql.MonadSql m
  ) =>
  Maybe Job.Model ->
  m ()
runJob maybeJob = case maybeJob of
  Nothing -> MonadSleep.sleep 1
  Just job -> do
    MonadLog.info $
      Text.unwords
        [ "starting job",
          Text.pack . show . Witch.into @Int.Int64 $ Model.key job,
          Text.pack . show . Job.task $ Model.value job
        ]
    case Job.task $ Model.value job of
      Task.ProcessHackageIndex -> HackageIndex.Process.run
      Task.UpsertHackageIndex -> HackageIndex.Upsert.run
      Task.Vacuum -> Database.Vacuum.run
    MonadLog.info $
      Text.unwords
        [ "finished job",
          Text.pack . show . Witch.into @Int.Int64 $ Model.key job
        ]
