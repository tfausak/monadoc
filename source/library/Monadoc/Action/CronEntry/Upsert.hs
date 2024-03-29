module Monadoc.Action.CronEntry.Upsert where

import qualified Data.Function as Function
import qualified Formatting as F
import qualified Monadoc.Action.App.Log as App.Log
import qualified Monadoc.Action.CronEntry.Insert as CronEntry.Insert
import qualified Monadoc.Action.CronEntry.Update as CronEntry.Update
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Query.CronEntry as CronEntry.Query
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model

run :: CronEntry.CronEntry -> App.App CronEntry.Model
run cronEntry =
  case cronEntry.guid of
    Nothing -> do
      model <- CronEntry.Insert.run cronEntry
      App.Log.info $ F.sformat ("inserting dynamic" F.%+ Key.format) model.key
      pure model
    Just guid -> do
      maybeCronEntry <- CronEntry.Query.getByGuid guid
      case maybeCronEntry of
        Nothing -> do
          model <- CronEntry.Insert.run cronEntry
          App.Log.info $ F.sformat ("inserted static" F.%+ Key.format) model.key
          pure model
        Just model -> do
          let existing = model.value
              differentSchedule = Function.on (/=) (.schedule) existing cronEntry
              differentTask = Function.on (/=) (.task) existing cronEntry
          if differentSchedule || differentTask
            then do
              let newModel = model {Model.value = cronEntry}
              CronEntry.Update.run newModel
              App.Log.info $ F.sformat ("updated static" F.%+ Key.format) newModel.key
              pure newModel
            else pure model
