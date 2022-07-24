{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.PackageMetaComponentModule.InsertSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.Blob.Insert as Blob.Insert
import qualified Monadoc.Action.Component.Insert as Component.Insert
import qualified Monadoc.Action.HackageUser.Upsert as HackageUser.Upsert
import qualified Monadoc.Action.License.Insert as License.Insert
import qualified Monadoc.Action.Module.Insert as Module.Insert
import qualified Monadoc.Action.Package.Upsert as Package.Upsert
import qualified Monadoc.Action.PackageMeta.Insert as PackageMeta.Insert
import qualified Monadoc.Action.PackageMetaComponent.Insert as PackageMetaComponent.Insert
import qualified Monadoc.Action.PackageMetaComponentModule.Insert as PackageMetaComponentModule.Insert
import qualified Monadoc.Action.Upload.Upsert as Upload.Upsert
import qualified Monadoc.Action.Version.Upsert as Version.Upsert
import qualified Monadoc.Model.Module as Module
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Model.PackageMetaComponent as PackageMetaComponent
import qualified Monadoc.Model.PackageMetaComponentModule as PackageMetaComponentModule
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.PackageMetaComponentModule.Insert" $ do
  Hspec.it "inserts a new package meta component module" . Test.run $ do
    packageMetaComponent <- insertPackageMetaComponent
    module_ <- insertModule
    packageMetaComponentModule <- Test.arbitraryWith $ \x ->
      x
        { PackageMetaComponentModule.packageMetaComponent = Model.key packageMetaComponent,
          PackageMetaComponentModule.module_ = Model.key module_
        }
    actual <-
      PackageMetaComponentModule.Insert.run packageMetaComponentModule
    let expected =
          Model.Model
            { Model.key = Witch.from @Int 1,
              Model.value = packageMetaComponentModule
            }
    IO.liftIO $ actual `Hspec.shouldBe` expected

insertPackageMetaComponentModule :: App.App PackageMetaComponentModule.Model
insertPackageMetaComponentModule = do
  packageMetaComponent <- insertPackageMetaComponent
  module_ <- insertModule
  packageMetaComponentModule <- Test.arbitraryWith $ \x ->
    x
      { PackageMetaComponentModule.packageMetaComponent = Model.key packageMetaComponent,
        PackageMetaComponentModule.module_ = Model.key module_
      }
  PackageMetaComponentModule.Insert.run packageMetaComponentModule

insertPackageMetaComponent :: App.App PackageMetaComponent.Model
insertPackageMetaComponent = do
  version <- do
    x <- Test.arbitrary
    Version.Upsert.run x
  license <- do
    x <- Test.arbitrary
    License.Insert.run x
  blob <- do
    x <- Test.arbitrary
    Blob.Insert.run x
  package <- do
    x <- Test.arbitrary
    Package.Upsert.run x
  hackageUser <- do
    x <- Test.arbitrary
    HackageUser.Upsert.run x
  upload <- do
    x <- Test.arbitraryWith $ \y ->
      y
        { Upload.blob = Model.key blob,
          Upload.package = Model.key package,
          Upload.uploadedBy = Model.key hackageUser,
          Upload.version = Model.key version
        }
    Upload.Upsert.run x
  packageMeta <- do
    x <- Test.arbitraryWith $ \y ->
      y
        { PackageMeta.cabalVersion = Model.key version,
          PackageMeta.license = Model.key license,
          PackageMeta.upload = Model.key upload
        }
    PackageMeta.Insert.run x
  component <- do
    x <- Test.arbitrary
    Component.Insert.run x
  x <- Test.arbitraryWith $ \y ->
    y
      { PackageMetaComponent.packageMeta = Model.key packageMeta,
        PackageMetaComponent.component = Model.key component
      }
  PackageMetaComponent.Insert.run x

insertModule :: App.App Module.Model
insertModule = do
  x <- Test.arbitrary
  Module.Insert.run x
