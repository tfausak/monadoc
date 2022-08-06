module Monadoc.Action.Job.Enqueue where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.MissingKey as MissingKey
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Model.Job as Job
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Status as Status
import qualified Monadoc.Type.Task as Task
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Witch

run :: Task.Task -> App.App Job.Model
run task = do
  now <- Timestamp.getCurrentTime
  let job =
        Job.Job
          { Job.createdAt = Witch.from now,
            Job.finishedAt = Nothing,
            Job.startedAt = Nothing,
            Job.status = Status.Queued,
            Job.task = task
          }
  rows <- App.Sql.query "insert into job (createdAt, finishedAt, startedAt, status, task) values (?, ?, ?, ?, ?) returning key" job
  case rows of
    [] -> Traced.throw MissingKey.MissingKey
    Sql.Only key : _ -> pure Model.Model {Model.key = key, Model.value = job}
