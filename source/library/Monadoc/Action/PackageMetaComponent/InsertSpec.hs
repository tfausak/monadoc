{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.PackageMetaComponent.InsertSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.Component.Insert as Component.Insert
import qualified Monadoc.Action.PackageMeta.InsertSpec as PackageMeta.InsertSpec
import qualified Monadoc.Action.PackageMetaComponent.Insert as PackageMetaComponent.Insert
import qualified Monadoc.Model.PackageMetaComponent as PackageMetaComponent
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.PackageMetaComponent.Insert" $ do
  Hspec.it "inserts a new package meta component" . Test.run $ do
    packageMeta <- PackageMeta.InsertSpec.insertPackageMeta
    component <- do
      x <- Test.arbitrary
      Component.Insert.run x
    packageMetaComponent <- Test.arbitraryWith $ \x ->
      x
        { PackageMetaComponent.packageMeta = Model.key packageMeta,
          PackageMetaComponent.component = Model.key component
        }
    actual <- PackageMetaComponent.Insert.run packageMetaComponent
    let expected =
          Model.Model
            { Model.key = Witch.from @Int 1,
              Model.value = packageMetaComponent
            }
    IO.liftIO $ actual `Hspec.shouldBe` expected
