module Monadoc.Action.PackageMetaComponentModule.Insert where

import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Model.PackageMetaComponentModule as PackageMetaComponentModule
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run ::
  PackageMetaComponentModule.PackageMetaComponentModule ->
  App.App PackageMetaComponentModule.Model
run packageMetaComponentModule = do
  App.Sql.execute
    "insert into packageMetaComponentModule (packageMetaComponent, module) \
    \ values (?, ?)"
    packageMetaComponentModule
  key <- Key.SelectLastInsert.run
  pure Model.Model {Model.key = key, Model.value = packageMetaComponentModule}
