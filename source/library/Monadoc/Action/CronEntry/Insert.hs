{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.CronEntry.Insert where

import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: CronEntry.CronEntry -> App.App CronEntry.Model
run cronEntry = do
  App.execute "insert into cronEntry (guid, runAt, schedule, task) values (?, ?, ?, ?)" cronEntry
  key <- Key.SelectLastInsert.run
  pure Model.Model {Model.key = key, Model.value = cronEntry}
