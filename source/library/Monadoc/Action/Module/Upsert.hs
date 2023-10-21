module Monadoc.Action.Module.Upsert where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.MissingKey as MissingKey
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Model.Module as Module
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: Module.Module -> App.App Module.Model
run module_ = do
  r1 <-
    App.Sql.query
      "select key from module where name = ? limit 1"
      [module_.name]
  key <- case r1 of
    Sql.Only key : _ -> pure key
    [] -> do
      r2 <- App.Sql.query "insert into module (name) values (?) returning key" module_
      case r2 of
        Sql.Only key : _ -> pure key
        [] -> Traced.throw MissingKey.MissingKey
  pure Model.Model {Model.key = key, Model.value = module_}
