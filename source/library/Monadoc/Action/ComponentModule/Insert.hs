{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.ComponentModule.Insert where

import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Model.ComponentModule as ComponentModule
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: ComponentModule.ComponentModule -> App.App ComponentModule.Model
run componentModule = do
  App.Sql.execute
    "insert into componentModule (component, module) values (?, ?)"
    componentModule
  key <- Key.SelectLastInsert.run
  pure Model.Model {Model.key = key, Model.value = componentModule}
