{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Job.Acquire where

import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Model.Job as Job
import qualified Monadoc.Query.Job as Job
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Status as Status
import qualified Monadoc.Type.Timestamp as Timestamp

run :: App.App (Maybe Job.Model)
run = do
  maybeJob <- Job.selectByStatus Status.Queued
  case maybeJob of
    Nothing -> pure ()
    Just job -> do
      now <- Timestamp.getCurrentTime
      App.Sql.execute
        "update job set startedAt = ?, status = ? where key = ?"
        (now, Status.Locked, Model.key job)
  pure maybeJob
