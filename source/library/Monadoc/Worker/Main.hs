{-# LANGUAGE TypeApplications #-}

module Monadoc.Worker.Main where

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.Database.Vacuum as Database.Vacuum
import qualified Monadoc.Action.HackageIndex.Process as HackageIndex.Process
import qualified Monadoc.Action.HackageIndex.Upsert as HackageIndex.Upsert
import qualified Monadoc.Action.Job.Enqueue as Job.Enqueue
import qualified Monadoc.Model.Job as Job
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Status as Status
import qualified Monadoc.Type.Task as Task
import qualified Monadoc.Vendor.Witch as Witch

worker :: Context.Context -> IO ()
worker = App.run $ do
  Monad.void $ Job.Enqueue.run Task.Vacuum
  Monad.void $ Job.Enqueue.run Task.UpsertHackageIndex
  Monad.void $ Job.Enqueue.run Task.ProcessHackageIndex

  Monad.forever $ do
    Exception.generalBracket acquireJob releaseJob runJob

acquireJob :: App.App (Maybe Job.Model)
acquireJob = App.withConnection $ \connection ->
  App.lift . Sql.withTransaction connection $ do
    rows <-
      Sql.query
        connection
        (Witch.into @Sql.Query "select * from job where status = ? order by createdAt asc limit 1")
        [Status.Queued]
    case rows of
      [] -> pure Nothing
      job : _ -> do
        now <- Time.getCurrentTime
        Sql.execute
          connection
          (Witch.into @Sql.Query "update job set startedAt = ?, status = ? where key = ?")
          (now, Status.Locked, Model.key job)
        pure $ Just job

releaseJob :: Maybe Job.Model -> Exception.ExitCase () -> App.App ()
releaseJob maybeJob exitCase = case maybeJob of
  Nothing -> pure ()
  Just job -> App.withConnection $ \connection -> App.lift $ do
    now <- Time.getCurrentTime
    let status = case exitCase of
          Exception.ExitCaseSuccess _ -> Status.Passed
          Exception.ExitCaseException _ -> Status.Failed
          Exception.ExitCaseAbort -> Status.Failed
    Sql.execute
      connection
      (Witch.into @Sql.Query "update job set finishedAt = ?, status = ? where key = ?")
      (now, status, Model.key job)

runJob :: Maybe Job.Model -> App.App ()
runJob maybeJob = case maybeJob of
  Nothing -> App.lift $ Concurrent.threadDelay 1000000
  Just job -> do
    App.sayString $ show job
    case Job.task $ Model.value job of
      Task.ProcessHackageIndex -> HackageIndex.Process.run
      Task.UpsertHackageIndex -> HackageIndex.Upsert.run
      Task.Vacuum -> Database.Vacuum.run
