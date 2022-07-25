module Monadoc.Action.PackageMetaComponent.Upsert where

import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.PackageMetaComponent.Insert as PackageMetaComponent.Insert
import qualified Monadoc.Model.PackageMetaComponent as PackageMetaComponent
import qualified Monadoc.Type.App as App

run ::
  PackageMetaComponent.PackageMetaComponent ->
  App.App PackageMetaComponent.Model
run component = do
  models <-
    App.Sql.query
      "select * \
      \ from packageMetaComponent \
      \ where packageMeta = ? \
      \ and component = ? \
      \ limit 1"
      ( PackageMetaComponent.packageMeta component,
        PackageMetaComponent.component component
      )
  case models of
    model : _ -> pure model
    [] -> PackageMetaComponent.Insert.run component
