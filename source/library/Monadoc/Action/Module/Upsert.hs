module Monadoc.Action.Module.Upsert where

import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.Module.Insert as Module.Insert
import qualified Monadoc.Model.Module as Module
import qualified Monadoc.Type.App as App

run :: Module.Module -> App.App Module.Model
run module_ = do
  models <- App.Sql.query "select * from module where name = ? limit 1" [Module.name module_]
  case models of
    model : _ -> pure model
    [] -> Module.Insert.run module_
