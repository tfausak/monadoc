{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Package.Upsert where

import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Query.Package as Package
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: Package.Package -> App.App Package.Model
run package = do
  maybeModel <- Package.selectByName $ Package.name package
  case maybeModel of
    Nothing -> do
      App.Sql.execute "insert into package (name) values (?)" package
      key <- Key.SelectLastInsert.run
      pure Model.Model {Model.key = key, Model.value = package}
    Just model -> pure model
