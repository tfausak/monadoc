{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.Job.Perform where

import qualified Control.Concurrent.Async as Async
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans.Control as Control
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Monadoc.Action.Task.Perform as Task.Perform
import qualified Monadoc.Class.MonadHttp as MonadHttp
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSleep as MonadSleep
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.Job as Job
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Witch

run ::
  ( Control.MonadBaseControl IO m,
    MonadHttp.MonadHttp m,
    MonadLog.MonadLog m,
    Exception.MonadMask m,
    Reader.MonadReader Context.Context m,
    MonadSleep.MonadSleep m,
    MonadSql.MonadSql m
  ) =>
  Maybe Job.Model ->
  m ()
run maybeJob = case maybeJob of
  Nothing -> MonadSleep.sleep 1
  Just job -> do
    MonadLog.info $
      Text.unwords
        [ "starting job",
          Text.pack . show . Witch.into @Int.Int64 $ Model.key job,
          Text.pack . show . Job.task $ Model.value job
        ]
    () <- Control.control $ \runInBase ->
      Async.withAsync (runInBase . Task.Perform.run . Job.task $ Model.value job) Async.wait
    MonadLog.info $
      Text.unwords
        [ "finished job",
          Text.pack . show . Witch.into @Int.Int64 $ Model.key job
        ]
