{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Job.Release where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Class.MonadTime as MonadTime
import qualified Monadoc.Model.Job as Job
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Status as Status
import qualified Monadoc.Type.Timestamp as Timestamp

run ::
  (MonadSql.MonadSql m, MonadTime.MonadTime m) =>
  Maybe Job.Model ->
  Exception.ExitCase () ->
  m ()
run maybeJob exitCase = case maybeJob of
  Nothing -> pure ()
  Just job -> do
    now <- Timestamp.getCurrentTime
    let status = case exitCase of
          Exception.ExitCaseSuccess _ -> Status.Passed
          Exception.ExitCaseException _ -> Status.Failed
          Exception.ExitCaseAbort -> Status.Failed
    MonadSql.execute
      "update job set finishedAt = ?, status = ? where key = ?"
      (now, status, Model.key job)
