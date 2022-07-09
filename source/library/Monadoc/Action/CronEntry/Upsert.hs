{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.CronEntry.Upsert where

import qualified Data.Function as Function
import qualified Monadoc.Action.CronEntry.Insert as CronEntry.Insert
import qualified Monadoc.Action.CronEntry.Update as CronEntry.Update
import qualified Monadoc.Action.Log as Log
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Query.CronEntry as CronEntry
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model
import qualified Witch

run :: CronEntry.CronEntry -> App.App CronEntry.Model
run cronEntry =
  case CronEntry.guid cronEntry of
    Nothing -> do
      model <- CronEntry.Insert.run cronEntry
      Log.info $ "inserted dynamic cron entry: " <> Witch.from (Model.key model)
      pure model
    Just guid -> do
      maybeCronEntry <- CronEntry.selectByGuid guid
      case maybeCronEntry of
        Nothing -> do
          model <- CronEntry.Insert.run cronEntry
          Log.info $ "inserted static cron entry: " <> Witch.from (Model.key model)
          pure model
        Just model -> do
          let existing = Model.value model
              differentSchedule = Function.on (/=) CronEntry.schedule existing cronEntry
              differentTask = Function.on (/=) CronEntry.task existing cronEntry
          if differentSchedule || differentTask
            then do
              let newModel = model {Model.value = cronEntry}
              CronEntry.Update.run newModel
              Log.debug $ "updated static cron entry: " <> Witch.from (Model.key newModel)
              pure newModel
            else pure model
