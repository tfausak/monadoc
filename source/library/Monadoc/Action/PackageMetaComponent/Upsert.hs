module Monadoc.Action.PackageMetaComponent.Upsert where

import qualified Monadoc.Action.PackageMetaComponent.Insert as PackageMetaComponent.Insert
import qualified Monadoc.Model.PackageMetaComponent as PackageMetaComponent
import qualified Monadoc.Query.PackageMetaComponent as PackageMetaComponent
import qualified Monadoc.Type.App as App

run ::
  PackageMetaComponent.PackageMetaComponent ->
  App.App PackageMetaComponent.Model
run component = do
  maybeModel <-
    PackageMetaComponent.selectByPackageMetaAndComponent
      (PackageMetaComponent.packageMeta component)
      (PackageMetaComponent.component component)
  case maybeModel of
    Just model -> pure model
    Nothing -> PackageMetaComponent.Insert.run component
