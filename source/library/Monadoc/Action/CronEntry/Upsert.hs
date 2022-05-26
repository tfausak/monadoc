{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.CronEntry.Upsert where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Function as Function
import qualified Monadoc.Action.CronEntry.Insert as CronEntry.Insert
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Query.CronEntry as CronEntry
import qualified Monadoc.Type.Model as Model
import qualified Witch

run ::
  (MonadLog.MonadLog m, MonadSql.MonadSql m, Exception.MonadThrow m) =>
  CronEntry.CronEntry ->
  m ()
run cronEntry =
  case CronEntry.guid cronEntry of
    Nothing -> MonadLog.warn $ "missing GUID on static cron entry: " <> Witch.from (show cronEntry)
    Just guid -> do
      maybeCronEntry <- CronEntry.selectByGuid guid
      case maybeCronEntry of
        Nothing -> do
          Monad.void $ CronEntry.Insert.run cronEntry
          MonadLog.info $ "inserted static cron entry: " <> Witch.from guid
        Just model -> do
          let existing = Model.value model
              differentSchedule = Function.on (/=) CronEntry.schedule existing cronEntry
              differentTask = Function.on (/=) CronEntry.task existing cronEntry
          Monad.when (differentSchedule || differentTask) $ do
            MonadSql.execute
              "update cronEntry set schedule = ?, task = ? where key = ?"
              (CronEntry.schedule cronEntry, CronEntry.task cronEntry, Model.key model)
            MonadLog.debug $ "updated static cron entry: " <> Witch.from guid
