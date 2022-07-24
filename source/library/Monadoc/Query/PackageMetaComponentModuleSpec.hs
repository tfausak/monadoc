module Monadoc.Query.PackageMetaComponentModuleSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.PackageMetaComponentModule.InsertSpec as PackageMetaComponentModule.InsertSpec
import qualified Monadoc.Model.PackageMetaComponentModule as PackageMetaComponentModule
import qualified Monadoc.Query.PackageMetaComponentModule as PackageMetaComponentModule
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Query.PackageMetaComponentModule" $ do
  Hspec.describe "selectByPackageMetaComponent" $ do
    Hspec.it "returns an empty list when there is no package meta component module" . Test.run $ do
      packageMetaComponent <- Test.arbitrary
      result <- PackageMetaComponentModule.selectByPackageMetaComponent packageMetaComponent
      IO.liftIO $ result `Hspec.shouldBe` []

    Hspec.it "returns a package meta component module when one exists" . Test.run $ do
      packageMetaComponentModule <- PackageMetaComponentModule.InsertSpec.insertPackageMetaComponentModule
      result <-
        PackageMetaComponentModule.selectByPackageMetaComponent
          . PackageMetaComponentModule.packageMetaComponent
          $ Model.value packageMetaComponentModule
      IO.liftIO $ result `Hspec.shouldBe` [packageMetaComponentModule]

  Hspec.describe "selectByPackageMetaComponentAndModule" $ do
    Hspec.it "returns nothing when there is no package meta component module" . Test.run $ do
      packageMetaComponent <- Test.arbitrary
      module_ <- Test.arbitrary
      result <- PackageMetaComponentModule.selectByPackageMetaComponentAndModule packageMetaComponent module_
      IO.liftIO $ result `Hspec.shouldBe` Nothing

    Hspec.it "returns a package meta component module when one exists" . Test.run $ do
      packageMetaComponentModule <- PackageMetaComponentModule.InsertSpec.insertPackageMetaComponentModule
      result <-
        PackageMetaComponentModule.selectByPackageMetaComponentAndModule
          (PackageMetaComponentModule.packageMetaComponent $ Model.value packageMetaComponentModule)
          (PackageMetaComponentModule.module_ $ Model.value packageMetaComponentModule)
      IO.liftIO $ result `Hspec.shouldBe` Just packageMetaComponentModule
