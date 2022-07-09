{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Migration.Insert where

import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run ::
  Migration.Migration ->
  App.App Migration.Model
run migration = do
  App.execute "insert into migration (createdAt, query) values (?, ?)" migration
  key <- Key.SelectLastInsert.run
  pure Model.Model {Model.key = key, Model.value = migration}
