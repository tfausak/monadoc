module Monadoc.Action.PackageMetaComponentModule.Upsert where

import qualified Monadoc.Action.PackageMetaComponentModule.Insert as PackageMetaComponentModule.Insert
import qualified Monadoc.Model.PackageMetaComponentModule as PackageMetaComponentModule
import qualified Monadoc.Query.PackageMetaComponentModule as PackageMetaComponentModule
import qualified Monadoc.Type.App as App

run ::
  PackageMetaComponentModule.PackageMetaComponentModule ->
  App.App PackageMetaComponentModule.Model
run packageMetaComponentModule = do
  maybeModel <-
    PackageMetaComponentModule.selectByPackageMetaComponentAndModule
      (PackageMetaComponentModule.packageMetaComponent packageMetaComponentModule)
      (PackageMetaComponentModule.module_ packageMetaComponentModule)
  case maybeModel of
    Just model -> pure model
    Nothing -> PackageMetaComponentModule.Insert.run packageMetaComponentModule
