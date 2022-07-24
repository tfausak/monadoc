{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Version.Upsert where

import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Query.Version as Version
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: Version.Version -> App.App Version.Model
run version = do
  maybeModel <- Version.selectByNumber $ Version.number version
  case maybeModel of
    Nothing -> do
      App.Sql.execute "insert into version (number) values (?)" version
      key <- Key.SelectLastInsert.run
      pure Model.Model {Model.key = key, Model.value = version}
    Just model -> pure model
