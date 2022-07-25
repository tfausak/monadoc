module Monadoc.Action.License.Upsert where

import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.License.Insert as License.Insert
import qualified Monadoc.Model.License as License
import qualified Monadoc.Type.App as App

run :: License.License -> App.App License.Model
run license = do
  models <- App.Sql.query "select * from license where spdx = ? limit 1" [License.spdx license]
  case models of
    model : _ -> pure model
    [] -> License.Insert.run license
