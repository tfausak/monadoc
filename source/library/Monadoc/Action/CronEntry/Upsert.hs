{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.CronEntry.Upsert where

import qualified Data.Function as Function
import qualified Formatting as F
import qualified Monadoc.Action.App.Log as App.Log
import qualified Monadoc.Action.CronEntry.Insert as CronEntry.Insert
import qualified Monadoc.Action.CronEntry.Update as CronEntry.Update
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Query.CronEntry as CronEntry
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model

run :: CronEntry.CronEntry -> App.App CronEntry.Model
run cronEntry =
  case CronEntry.guid cronEntry of
    Nothing -> do
      model <- CronEntry.Insert.run cronEntry
      App.Log.info $ F.sformat ("inserting dynamic " F.% Key.format) (Model.key model)
      pure model
    Just guid -> do
      maybeCronEntry <- CronEntry.selectByGuid guid
      case maybeCronEntry of
        Nothing -> do
          model <- CronEntry.Insert.run cronEntry
          App.Log.info $ F.sformat ("inserted static " F.% Key.format) (Model.key model)
          pure model
        Just model -> do
          let existing = Model.value model
              differentSchedule = Function.on (/=) CronEntry.schedule existing cronEntry
              differentTask = Function.on (/=) CronEntry.task existing cronEntry
          if differentSchedule || differentTask
            then do
              let newModel = model {Model.value = cronEntry}
              CronEntry.Update.run newModel
              App.Log.info $ F.sformat ("updated static " F.% Key.format) (Model.key newModel)
              pure newModel
            else pure model
