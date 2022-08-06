module Monadoc.Action.PackageMetaComponent.Upsert where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.MissingKey as MissingKey
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Model.PackageMetaComponent as PackageMetaComponent
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: PackageMetaComponent.PackageMetaComponent -> App.App PackageMetaComponent.Model
run packageMetaComponent = do
  r1 <-
    App.Sql.query
      "select key from packageMetaComponent where packageMeta = ? and component = ? limit 1"
      (PackageMetaComponent.packageMeta packageMetaComponent, PackageMetaComponent.component packageMetaComponent)
  key <- case r1 of
    Sql.Only key : _ -> pure key
    [] -> do
      r2 <- App.Sql.query "insert into packageMetaComponent (packageMeta, component) values (?, ?) returning key" packageMetaComponent
      case r2 of
        Sql.Only key : _ -> pure key
        [] -> Traced.throw MissingKey.MissingKey
  pure Model.Model {Model.key = key, Model.value = packageMetaComponent}
