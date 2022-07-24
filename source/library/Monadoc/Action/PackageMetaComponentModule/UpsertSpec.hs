{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.PackageMetaComponentModule.UpsertSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.PackageMetaComponentModule.InsertSpec as PackageMetaComponentModule.InsertSpec
import qualified Monadoc.Action.PackageMetaComponentModule.Upsert as PackageMetaComponentModule.Upsert
import qualified Monadoc.Model.PackageMetaComponentModule as PackageMetaComponentModule
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.PackageMetaComponentModule.Upsert" $ do
  Hspec.it "inserts a new package meta component module" . Test.run $ do
    packageMetaComponent <- PackageMetaComponentModule.InsertSpec.insertPackageMetaComponent
    module_ <- PackageMetaComponentModule.InsertSpec.insertModule
    packageMetaComponentModule <- Test.arbitraryWith $ \x ->
      x
        { PackageMetaComponentModule.packageMetaComponent = Model.key packageMetaComponent,
          PackageMetaComponentModule.module_ = Model.key module_
        }
    actual <- PackageMetaComponentModule.Upsert.run packageMetaComponentModule
    let expected =
          Model.Model
            { Model.key = Witch.from @Int 1,
              Model.value = packageMetaComponentModule
            }
    IO.liftIO $ actual `Hspec.shouldBe` expected

  Hspec.it "updates an existing package meta component module" . Test.run $ do
    packageMetaComponent <- PackageMetaComponentModule.InsertSpec.insertPackageMetaComponent
    module_ <- PackageMetaComponentModule.InsertSpec.insertModule
    packageMetaComponentModule <- Test.arbitraryWith $ \x ->
      x
        { PackageMetaComponentModule.packageMetaComponent = Model.key packageMetaComponent,
          PackageMetaComponentModule.module_ = Model.key module_
        }
    old <- PackageMetaComponentModule.Upsert.run packageMetaComponentModule
    new <- PackageMetaComponentModule.Upsert.run packageMetaComponentModule
    IO.liftIO $ new `Hspec.shouldBe` old

  Hspec.it "inserts two package meta component modules" . Test.run $ do
    packageMetaComponent <- PackageMetaComponentModule.InsertSpec.insertPackageMetaComponent
    module1 <- PackageMetaComponentModule.InsertSpec.insertModule
    packageMetaComponentModule1 <- Test.arbitraryWith $ \x ->
      x
        { PackageMetaComponentModule.packageMetaComponent = Model.key packageMetaComponent,
          PackageMetaComponentModule.module_ = Model.key module1
        }
    model1 <- PackageMetaComponentModule.Upsert.run packageMetaComponentModule1
    module2 <- PackageMetaComponentModule.InsertSpec.insertModule
    packageMetaComponentModule2 <- Test.arbitraryWith $ \x ->
      x
        { PackageMetaComponentModule.packageMetaComponent = Model.key packageMetaComponent,
          PackageMetaComponentModule.module_ = Model.key module2
        }
    model2 <- PackageMetaComponentModule.Upsert.run packageMetaComponentModule2
    IO.liftIO $ Model.key model1 `Hspec.shouldNotBe` Model.key model2
