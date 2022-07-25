module Monadoc.Action.PackageMetaComponentModule.Upsert where

import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.PackageMetaComponentModule.Insert as PackageMetaComponentModule.Insert
import qualified Monadoc.Model.PackageMetaComponentModule as PackageMetaComponentModule
import qualified Monadoc.Type.App as App

run ::
  PackageMetaComponentModule.PackageMetaComponentModule ->
  App.App PackageMetaComponentModule.Model
run packageMetaComponentModule = do
  models <-
    App.Sql.query
      "select * \
      \ from packageMetaComponentModule \
      \ where packageMetaComponent = ? \
      \ and module = ? \
      \ limit 1"
      ( PackageMetaComponentModule.packageMetaComponent packageMetaComponentModule,
        PackageMetaComponentModule.module_ packageMetaComponentModule
      )
  case models of
    model : _ -> pure model
    [] -> PackageMetaComponentModule.Insert.run packageMetaComponentModule
