module Monadoc.Action.CronEntry.Upsert where

import qualified Control.Monad as Monad
import qualified Data.Function as Function
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Type.Model as Model
import qualified Witch

run ::
  (MonadLog.MonadLog m, MonadSql.MonadSql m) =>
  CronEntry.CronEntry ->
  m ()
run cronEntry =
  case CronEntry.guid cronEntry of
    Nothing -> MonadLog.warn $ "missing GUID on static cron entry: " <> Witch.from (show cronEntry)
    Just guid -> do
      models <- MonadSql.query "select * from cronEntry where guid = ?" [guid]
      case models of
        [] -> do
          MonadSql.execute "insert into cronEntry (guid, runAt, schedule, task) values (?, ?, ?, ?)" cronEntry
          MonadLog.info $ "inserted static cron entry: " <> Witch.from guid
        model : _ -> do
          let existing = Model.value model
              differentSchedule = Function.on (/=) CronEntry.schedule existing cronEntry
              differentTask = Function.on (/=) CronEntry.task existing cronEntry
          Monad.when (differentSchedule || differentTask) $ do
            MonadSql.execute
              "update cronEntry set schedule = ?, task = ? where key = ?"
              (CronEntry.schedule cronEntry, CronEntry.task cronEntry, Model.key model)
            MonadLog.debug $ "updated static cron entry: " <> Witch.from guid
