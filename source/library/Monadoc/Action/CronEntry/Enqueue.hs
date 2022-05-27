{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.CronEntry.Enqueue where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Action.CronEntry.Update as CronEntry.Update
import qualified Monadoc.Action.Job.Enqueue as Job.Enqueue
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Exception.NoNextMatch as NoNextMatch
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Query.CronEntry as CronEntry
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified System.Cron as Cron
import qualified Witch

run ::
  ( MonadLog.MonadLog m,
    MonadSql.MonadSql m,
    Exception.MonadThrow m
  ) =>
  m ()
run = do
  now <- Timestamp.getCurrentTime
  cronEntries <- CronEntry.selectByRunAt now
  Monad.forM_ cronEntries $ \cronEntry -> do
    job <- Job.Enqueue.run . CronEntry.task $ Model.value cronEntry
    MonadLog.debug $
      "enqueued cron entry "
        <> Witch.from (Model.key cronEntry)
        <> " as job "
        <> Witch.from (Model.key job)
    let schedule = CronEntry.schedule $ Model.value cronEntry
    case Cron.nextMatch (Witch.from schedule) (Witch.from now) of
      Nothing -> Exception.throwM $ NoNextMatch.NoNextMatch now schedule
      Just nextMatch ->
        CronEntry.Update.run
          cronEntry
            { Model.value =
                (Model.value cronEntry)
                  { CronEntry.runAt = Witch.into @Timestamp.Timestamp nextMatch
                  }
            }
