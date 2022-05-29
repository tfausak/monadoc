{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.CronEntry.Upsert where

import qualified Control.Monad.Catch as Exception
import qualified Data.Function as Function
import qualified Monadoc.Action.CronEntry.Insert as CronEntry.Insert
import qualified Monadoc.Action.CronEntry.Update as CronEntry.Update
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Query.CronEntry as CronEntry
import qualified Monadoc.Type.Model as Model
import qualified Witch

run ::
  (MonadLog.MonadLog m, MonadSql.MonadSql m, Exception.MonadThrow m) =>
  CronEntry.CronEntry ->
  m CronEntry.Model
run cronEntry =
  case CronEntry.guid cronEntry of
    Nothing -> do
      model <- CronEntry.Insert.run cronEntry
      MonadLog.info $ "inserted dynamic cron entry: " <> Witch.from (Model.key model)
      pure model
    Just guid -> do
      maybeCronEntry <- CronEntry.selectByGuid guid
      case maybeCronEntry of
        Nothing -> do
          model <- CronEntry.Insert.run cronEntry
          MonadLog.info $ "inserted static cron entry: " <> Witch.from (Model.key model)
          pure model
        Just model -> do
          let existing = Model.value model
              differentSchedule = Function.on (/=) CronEntry.schedule existing cronEntry
              differentTask = Function.on (/=) CronEntry.task existing cronEntry
          if differentSchedule || differentTask
            then do
              let newModel = model {Model.value = cronEntry}
              CronEntry.Update.run newModel
              MonadLog.debug $ "updated static cron entry: " <> Witch.from (Model.key newModel)
              pure newModel
            else pure model
