module Monadoc.Action.Job.Prune where

import qualified Control.Monad as Monad
import qualified Data.Time as Time
import qualified Formatting as F
import qualified Monadoc.Action.App.Log as App.Log
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Model.Job as Job
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Status as Status
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Witch

run :: App.App ()
run = do
  now <- Timestamp.getCurrentTime
  let timestamp =
        Witch.into @Timestamp.Timestamp
          . Time.addUTCTime (-86400)
          $ Witch.into @Time.UTCTime now
  jobs <-
    App.Sql.query @Job.Model
      "select * from job where status = ? and createdAt < ?"
      (Status.Passed, timestamp)
  Monad.forM_ jobs $ \job -> do
    App.Log.debug $ F.sformat ("pruning" F.%+ Key.format) job.key
    App.Sql.execute "delete from job where key = ?" [job.key]
