module Monadoc.Action.Job.Acquire where

import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Class.MonadTime as MonadTime
import qualified Monadoc.Model.Job as Job
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Status as Status

run :: (MonadSql.MonadSql m, MonadTime.MonadTime m) => m (Maybe Job.Model)
run = do
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
