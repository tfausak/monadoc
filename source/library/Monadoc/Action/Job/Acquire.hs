module Monadoc.Action.Job.Acquire where

import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Model.Job as Job
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Status as Status
import qualified Monadoc.Type.Timestamp as Timestamp

run :: App.App (Maybe Job.Model)
run = do
  jobs <- App.Sql.query "select * from job where status = ? order by createdAt asc limit 1" [Status.Queued]
  case jobs of
    [] -> pure Nothing
    job : _ -> do
      now <- Timestamp.getCurrentTime
      App.Sql.execute
        "update job set startedAt = ?, status = ? where key = ?"
        (now, Status.Locked, job.key)
      pure $ Just job
