module Monadoc.Action.PackageMetaComponent.UpsertSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.PackageMetaComponent.Upsert as PackageMetaComponent.Upsert
import qualified Monadoc.Factory as Factory
import qualified Monadoc.Model.PackageMetaComponent as PackageMetaComponent
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.PackageMetaComponent.Upsert" $ do
  Hspec.it "inserts a new component" . Test.run $ do
    packageMeta <- Factory.newPackageMeta
    component <- Factory.newComponent
    packageMetaComponent <- Test.arbitraryWith $ \x ->
      x
        { PackageMetaComponent.packageMeta = packageMeta.key,
          PackageMetaComponent.component = component.key
        }
    model <- PackageMetaComponent.Upsert.run packageMetaComponent
    IO.liftIO $
      model
        `Hspec.shouldBe` Model.Model
          { Model.key = Witch.from @Int 1,
            Model.value = packageMetaComponent
          }

  Hspec.it "updates an existing component" . Test.run $ do
    packageMeta <- Factory.newPackageMeta
    component <- Factory.newComponent
    packageMetaComponent <- Test.arbitraryWith $ \x ->
      x
        { PackageMetaComponent.packageMeta = packageMeta.key,
          PackageMetaComponent.component = component.key
        }
    old <- PackageMetaComponent.Upsert.run packageMetaComponent
    new <- PackageMetaComponent.Upsert.run packageMetaComponent
    IO.liftIO $ new `Hspec.shouldBe` old

  Hspec.it "inserts two components" . Test.run $ do
    packageMeta <- Factory.newPackageMeta
    component1 <- Factory.newComponent
    packageMetaComponent1 <- Test.arbitraryWith $ \x ->
      x
        { PackageMetaComponent.packageMeta = packageMeta.key,
          PackageMetaComponent.component = component1.key
        }
    model1 <- PackageMetaComponent.Upsert.run packageMetaComponent1
    component2 <- Factory.newComponent
    packageMetaComponent2 <- Test.arbitraryWith $ \x ->
      x
        { PackageMetaComponent.packageMeta = packageMeta.key,
          PackageMetaComponent.component = component2.key
        }
    model2 <- PackageMetaComponent.Upsert.run packageMetaComponent2
    IO.liftIO $ model1.key `Hspec.shouldNotBe` model2.key
