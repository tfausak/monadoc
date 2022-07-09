{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.Job.Perform where

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Control as Control
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Monadoc.Action.Log as Log
import qualified Monadoc.Action.Task.Perform as Task.Perform
import qualified Monadoc.Model.Job as Job
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model
import qualified Witch

run :: Maybe Job.Model -> App.App ()
run maybeJob = case maybeJob of
  Nothing -> Trans.lift $ Concurrent.threadDelay 1000000
  Just job -> do
    Log.info $
      Text.unwords
        [ "starting job",
          Text.pack . show . Witch.into @Int.Int64 $ Model.key job,
          Text.pack . show . Job.task $ Model.value job
        ]
    () <- Control.control $ \runInBase ->
      Async.withAsync (runInBase . Task.Perform.run . Job.task $ Model.value job) Async.wait
    Log.info $
      Text.unwords
        [ "finished job",
          Text.pack . show . Witch.into @Int.Int64 $ Model.key job
        ]
