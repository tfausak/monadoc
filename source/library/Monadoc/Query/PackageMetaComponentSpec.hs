module Monadoc.Query.PackageMetaComponentSpec where

import qualified Control.Monad.Base as Base
import qualified Monadoc.Action.Component.Insert as Component.Insert
import qualified Monadoc.Action.PackageMeta.InsertSpec as PackageMeta.InsertSpec
import qualified Monadoc.Action.PackageMetaComponent.Insert as PackageMetaComponent.Insert
import qualified Monadoc.Model.PackageMetaComponent as PackageMetaComponent
import qualified Monadoc.Query.PackageMetaComponent as PackageMetaComponent
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Query.PackageMetaComponent" . Hspec.around Test.withConnection $ do
  Hspec.describe "selectByPackageMeta" $ do
    Hspec.it "works when there is no package meta component" . Test.runFake $ do
      packageMeta <- Test.arbitrary
      result <- PackageMetaComponent.selectByPackageMeta packageMeta
      Base.liftBase $ result `Hspec.shouldBe` []

    Hspec.it "works when there is a package meta component" . Test.runFake $ do
      packageMeta <- PackageMeta.InsertSpec.insertPackageMeta
      component <- do
        x <- Test.arbitrary
        Component.Insert.run x
      packageMetaComponent <- Test.arbitraryWith $ \x ->
        x
          { PackageMetaComponent.packageMeta = Model.key packageMeta,
            PackageMetaComponent.component = Model.key component
          }
      model <- PackageMetaComponent.Insert.run packageMetaComponent
      result <- PackageMetaComponent.selectByPackageMeta $ Model.key packageMeta
      Base.liftBase $ result `Hspec.shouldBe` [model]

  Hspec.describe "selectByPackageMetaAndComponent" $ do
    Hspec.it "works when there is no package meta component" . Test.runFake $ do
      packageMeta <- Test.arbitrary
      component <- Test.arbitrary
      result <- PackageMetaComponent.selectByPackageMetaAndComponent packageMeta component
      Base.liftBase $ result `Hspec.shouldBe` Nothing

    Hspec.it "works when there is a package meta component" . Test.runFake $ do
      packageMeta <- PackageMeta.InsertSpec.insertPackageMeta
      component <- do
        x <- Test.arbitrary
        Component.Insert.run x
      packageMetaComponent <- Test.arbitraryWith $ \x ->
        x
          { PackageMetaComponent.packageMeta = Model.key packageMeta,
            PackageMetaComponent.component = Model.key component
          }
      model <- PackageMetaComponent.Insert.run packageMetaComponent
      result <-
        PackageMetaComponent.selectByPackageMetaAndComponent
          (Model.key packageMeta)
          (Model.key component)
      Base.liftBase $ result `Hspec.shouldBe` Just model
