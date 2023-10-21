module Monadoc.Action.PackageMetaComponentModule.Upsert where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.MissingKey as MissingKey
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Model.PackageMetaComponentModule as PackageMetaComponentModule
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: PackageMetaComponentModule.PackageMetaComponentModule -> App.App PackageMetaComponentModule.Model
run packageMetaComponentModule = do
  r1 <-
    App.Sql.query
      "select key from packageMetaComponentModule where packageMetaComponent = ? and module = ? limit 1"
      (packageMetaComponentModule.packageMetaComponent, packageMetaComponentModule.module_)
  key <- case r1 of
    Sql.Only key : _ -> pure key
    [] -> do
      r2 <- App.Sql.query "insert into packageMetaComponentModule (packageMetaComponent, module) values (?, ?) returning key" packageMetaComponentModule
      case r2 of
        Sql.Only key : _ -> pure key
        [] -> Traced.throw MissingKey.MissingKey
  pure Model.Model {Model.key = key, Model.value = packageMetaComponentModule}
