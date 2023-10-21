module Monadoc.Action.Job.Release where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Model.Job as Job
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Status as Status
import qualified Monadoc.Type.Timestamp as Timestamp

run ::
  Maybe Job.Model ->
  Exception.ExitCase () ->
  App.App ()
run maybeJob exitCase = case maybeJob of
  Nothing -> pure ()
  Just job -> do
    now <- Timestamp.getCurrentTime
    let status = case exitCase of
          Exception.ExitCaseSuccess _ -> Status.Passed
          Exception.ExitCaseException _ -> Status.Failed
          Exception.ExitCaseAbort -> Status.Failed
    App.Sql.execute
      "update job set finishedAt = ?, status = ? where key = ?"
      (now, status, job.key)
