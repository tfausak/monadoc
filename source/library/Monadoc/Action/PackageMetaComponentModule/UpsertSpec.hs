module Monadoc.Action.PackageMetaComponentModule.UpsertSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.PackageMetaComponentModule.Upsert as PackageMetaComponentModule.Upsert
import qualified Monadoc.Factory as Factory
import qualified Monadoc.Model.PackageMetaComponentModule as PackageMetaComponentModule
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.PackageMetaComponentModule.Upsert" $ do
  Hspec.it "inserts a new package meta component module" . Test.run $ do
    packageMetaComponent <- Factory.newPackageMetaComponent
    module_ <- Factory.newModule
    packageMetaComponentModule <- Test.arbitraryWith $ \x ->
      x
        { PackageMetaComponentModule.packageMetaComponent = packageMetaComponent.key,
          PackageMetaComponentModule.module_ = module_.key
        }
    actual <- PackageMetaComponentModule.Upsert.run packageMetaComponentModule
    let expected =
          Model.Model
            { Model.key = Witch.from @Int 1,
              Model.value = packageMetaComponentModule
            }
    IO.liftIO $ actual `Hspec.shouldBe` expected

  Hspec.it "updates an existing package meta component module" . Test.run $ do
    packageMetaComponent <- Factory.newPackageMetaComponent
    module_ <- Factory.newModule
    packageMetaComponentModule <- Test.arbitraryWith $ \x ->
      x
        { PackageMetaComponentModule.packageMetaComponent = packageMetaComponent.key,
          PackageMetaComponentModule.module_ = module_.key
        }
    old <- PackageMetaComponentModule.Upsert.run packageMetaComponentModule
    new <- PackageMetaComponentModule.Upsert.run packageMetaComponentModule
    IO.liftIO $ new `Hspec.shouldBe` old

  Hspec.it "inserts two package meta component modules" . Test.run $ do
    packageMetaComponent <- Factory.newPackageMetaComponent
    module1 <- Factory.newModule
    packageMetaComponentModule1 <- Test.arbitraryWith $ \x ->
      x
        { PackageMetaComponentModule.packageMetaComponent = packageMetaComponent.key,
          PackageMetaComponentModule.module_ = module1.key
        }
    model1 <- PackageMetaComponentModule.Upsert.run packageMetaComponentModule1
    module2 <- Factory.newModule
    packageMetaComponentModule2 <- Test.arbitraryWith $ \x ->
      x
        { PackageMetaComponentModule.packageMetaComponent = packageMetaComponent.key,
          PackageMetaComponentModule.module_ = module2.key
        }
    model2 <- PackageMetaComponentModule.Upsert.run packageMetaComponentModule2
    IO.liftIO $ model1.key `Hspec.shouldNotBe` model2.key
