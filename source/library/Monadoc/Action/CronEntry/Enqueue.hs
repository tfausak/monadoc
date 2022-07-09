{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.CronEntry.Enqueue where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Action.CronEntry.Update as CronEntry.Update
import qualified Monadoc.Action.Job.Enqueue as Job.Enqueue
import qualified Monadoc.Action.Log as Log
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Class.MonadTime as MonadTime
import qualified Monadoc.Exception.NoNextMatch as NoNextMatch
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Model.Job as Job
import qualified Monadoc.Query.CronEntry as CronEntry
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified System.Cron as Cron
import qualified Witch

run :: App.App ()
run = do
  m <- maybeRunOne
  case m of
    Nothing -> pure ()
    Just (job, cronEntry) -> do
      Log.debug $
        "enqueued cron entry "
          <> Witch.from (Model.key cronEntry)
          <> " as job "
          <> Witch.from (Model.key job)
      run

maybeRunOne ::
  (MonadSql.MonadSql m, MonadTime.MonadTime m, Exception.MonadThrow m) =>
  m (Maybe (Job.Model, CronEntry.Model))
maybeRunOne = do
  now <- Timestamp.getCurrentTime
  maybeCronEntry <- CronEntry.selectNext now
  case maybeCronEntry of
    Nothing -> pure Nothing
    Just cronEntry -> Just <$> runOne cronEntry

runOne ::
  (MonadSql.MonadSql m, MonadTime.MonadTime m, Exception.MonadThrow m) =>
  CronEntry.Model ->
  m (Job.Model, CronEntry.Model)
runOne cronEntry = do
  job <- enqueueJob cronEntry
  newCronEntry <- updateRunAt cronEntry
  pure (job, newCronEntry)

enqueueJob ::
  (MonadSql.MonadSql m, MonadTime.MonadTime m, Exception.MonadThrow m) =>
  CronEntry.Model ->
  m Job.Model
enqueueJob = Job.Enqueue.run . CronEntry.task . Model.value

updateRunAt ::
  (MonadSql.MonadSql m, MonadTime.MonadTime m, Exception.MonadThrow m) =>
  CronEntry.Model ->
  m CronEntry.Model
updateRunAt cronEntry = do
  now <- Timestamp.getCurrentTime
  runAt <- nextRunAt now cronEntry
  let newCronEntry = cronEntry {Model.value = (Model.value cronEntry) {CronEntry.runAt = runAt}}
  CronEntry.Update.run newCronEntry
  pure newCronEntry

nextRunAt ::
  (MonadSql.MonadSql m, Exception.MonadThrow m) =>
  Timestamp.Timestamp ->
  CronEntry.Model ->
  m Timestamp.Timestamp
nextRunAt now cronEntry = do
  let schedule = CronEntry.schedule $ Model.value cronEntry
  case Cron.nextMatch (Witch.from schedule) (Witch.from now) of
    Nothing -> Exception.throwM $ NoNextMatch.NoNextMatch now schedule
    Just nextMatch -> pure $ Witch.into @Timestamp.Timestamp nextMatch
