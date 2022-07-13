{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Component.Insert where

import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: Component.Component -> App.App Component.Model
run component = do
  App.Sql.execute "insert into component (type, name) values (?, ?)" component
  key <- Key.SelectLastInsert.run
  pure Model.Model {Model.key = key, Model.value = component}
