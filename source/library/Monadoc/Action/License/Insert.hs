{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.License.Insert where

import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Model.License as License
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: License.License -> App.App License.Model
run license = do
  App.Sql.execute "insert into license (spdx) values (?)" license
  key <- Key.SelectLastInsert.run
  pure Model.Model {Model.key = key, Model.value = license}
