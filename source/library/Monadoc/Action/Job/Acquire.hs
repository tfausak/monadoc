{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Job.Acquire where

import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Class.MonadTime as MonadTime
import qualified Monadoc.Model.Job as Job
import qualified Monadoc.Query.Job as Job
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Status as Status
import qualified Monadoc.Type.Timestamp as Timestamp

run :: (MonadSql.MonadSql m, MonadTime.MonadTime m) => m (Maybe Job.Model)
run = do
  maybeJob <- Job.selectByStatus Status.Queued
  case maybeJob of
    Nothing -> pure ()
    Just job -> do
      now <- Timestamp.getCurrentTime
      MonadSql.execute
        "update job set startedAt = ?, status = ? where key = ?"
        (now, Status.Locked, Model.key job)
  pure maybeJob
