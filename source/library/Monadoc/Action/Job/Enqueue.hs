{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.Job.Enqueue where

import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans as Trans
import qualified Data.Pool as Pool
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Extra.SqliteSimple as Sql
import qualified Monadoc.Model.Job as Job
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Status as Status
import qualified Monadoc.Type.Task as Task
import qualified Monadoc.Vendor.Witch as Witch

run :: Task.Task -> App.App Job.Model
run task = do
  context <- Reader.ask
  Pool.withResource (Context.pool context) $ \connection -> Trans.lift $ do
    now <- Time.getCurrentTime
    let job =
          Job.Job
            { Job.createdAt = now,
              Job.finishedAt = Nothing,
              Job.startedAt = Nothing,
              Job.status = Status.Queued,
              Job.task = task
            }
    Sql.execute
      connection
      (Witch.into @Sql.Query "insert into job (createdAt, finishedAt, startedAt, status, task) values (?, ?, ?, ?, ?)")
      job
    key <- Sql.selectLastInsertRowid connection
    pure Model.Model {Model.key = Witch.into @Job.Key key, Model.value = job}
