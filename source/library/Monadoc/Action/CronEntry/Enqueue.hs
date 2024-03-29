module Monadoc.Action.CronEntry.Enqueue where

import qualified Formatting as F
import qualified Monadoc.Action.App.Log as App.Log
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.CronEntry.Update as CronEntry.Update
import qualified Monadoc.Action.Job.Enqueue as Job.Enqueue
import qualified Monadoc.Exception.NoNextMatch as NoNextMatch
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Model.Job as Job
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Saturn
import qualified Witch

run :: App.App ()
run = do
  m <- maybeRunOne
  case m of
    Nothing -> pure ()
    Just (job, cronEntry) -> do
      App.Log.debug $
        F.sformat
          ("[worker] enqueued" F.%+ Key.format F.%+ "as" F.%+ Key.format)
          cronEntry.key
          job.key
      run

maybeRunOne :: App.App (Maybe (Job.Model, CronEntry.Model))
maybeRunOne = do
  now <- Timestamp.getCurrentTime
  cronEntries <- App.Sql.query "select * from cronEntry where runAt <= ? order by runAt asc limit 1" [now]
  case cronEntries of
    [] -> pure Nothing
    cronEntry : _ -> Just <$> runOne cronEntry

runOne :: CronEntry.Model -> App.App (Job.Model, CronEntry.Model)
runOne cronEntry = do
  job <- enqueueJob cronEntry
  newCronEntry <- updateRunAt cronEntry
  pure (job, newCronEntry)

enqueueJob :: CronEntry.Model -> App.App Job.Model
enqueueJob = Job.Enqueue.run . (.value.task)

updateRunAt :: CronEntry.Model -> App.App CronEntry.Model
updateRunAt cronEntry = do
  now <- Timestamp.getCurrentTime
  runAt <- nextRunAt now cronEntry
  let newCronEntry = cronEntry {Model.value = cronEntry.value {CronEntry.runAt = runAt}}
  CronEntry.Update.run newCronEntry
  pure newCronEntry

nextRunAt :: Timestamp.Timestamp -> CronEntry.Model -> App.App Timestamp.Timestamp
nextRunAt now cronEntry = do
  let schedule = cronEntry.value.schedule
  case Saturn.nextMatch (Witch.from now) (Witch.from schedule) of
    Nothing -> Traced.throw $ NoNextMatch.NoNextMatch now schedule
    Just nextMatch -> pure $ Witch.into @Timestamp.Timestamp nextMatch
