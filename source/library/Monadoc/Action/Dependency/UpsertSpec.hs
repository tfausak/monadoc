module Monadoc.Action.Dependency.UpsertSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.Dependency.Upsert as Dependency.Upsert
import qualified Monadoc.Factory as Factory
import qualified Monadoc.Model.Dependency as Dependency
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Dependency.Upsert" $ do
  Hspec.it "inserts a new dependency" . Test.run $ do
    packageMetaComponent <- Factory.newPackageMetaComponent
    package <- Factory.newPackage
    component <- Factory.newComponent
    range <- Factory.newRange
    dependency <- Test.arbitraryWith $ \x ->
      x
        { Dependency.packageMetaComponent = Model.key packageMetaComponent,
          Dependency.package = Model.key package,
          Dependency.component = Model.key component,
          Dependency.range = Model.key range
        }
    actual <- Dependency.Upsert.run dependency
    let expected = Model.Model {Model.key = Witch.from @Int 1, Model.value = dependency}
    IO.liftIO $ actual `Hspec.shouldBe` expected

  Hspec.it "updates an existing dependency" . Test.run $ do
    packageMetaComponent <- Factory.newPackageMetaComponent
    package <- Factory.newPackage
    component <- Factory.newComponent
    range <- Factory.newRange
    dependency <- Test.arbitraryWith $ \x ->
      x
        { Dependency.packageMetaComponent = Model.key packageMetaComponent,
          Dependency.package = Model.key package,
          Dependency.component = Model.key component,
          Dependency.range = Model.key range
        }
    old <- Dependency.Upsert.run dependency
    new <- Dependency.Upsert.run dependency
    IO.liftIO $ new `Hspec.shouldBe` old

  Hspec.it "inserts two dependencies" . Test.run $ do
    packageMetaComponent <- Factory.newPackageMetaComponent
    package <- Factory.newPackage
    range <- Factory.newRange
    component1 <- Factory.newComponent
    dependency1 <- Test.arbitraryWith $ \x ->
      x
        { Dependency.packageMetaComponent = Model.key packageMetaComponent,
          Dependency.package = Model.key package,
          Dependency.component = Model.key component1,
          Dependency.range = Model.key range
        }
    model1 <- Dependency.Upsert.run dependency1
    component2 <- Factory.newComponent
    dependency2 <- Test.arbitraryWith $ \x ->
      x
        { Dependency.packageMetaComponent = Model.key packageMetaComponent,
          Dependency.package = Model.key package,
          Dependency.component = Model.key component2,
          Dependency.range = Model.key range
        }
    model2 <- Dependency.Upsert.run dependency2
    IO.liftIO $ Model.key model2 `Hspec.shouldNotBe` Model.key model1
