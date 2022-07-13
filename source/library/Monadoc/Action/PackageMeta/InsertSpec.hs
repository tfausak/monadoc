{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.PackageMeta.InsertSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.Blob.Upsert as Blob.Upsert
import qualified Monadoc.Action.HackageUser.Upsert as HackageUser.Upsert
import qualified Monadoc.Action.License.Upsert as License.Upsert
import qualified Monadoc.Action.Package.Upsert as Package.Upsert
import qualified Monadoc.Action.PackageMeta.Insert as PackageMeta.Insert
import qualified Monadoc.Action.Upload.Upsert as Upload.Upsert
import qualified Monadoc.Action.Version.Upsert as Version.Upsert
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.PackageMeta.Insert" $ do
  Hspec.it "inserts a new package meta" . Test.run $ do
    packageMeta <- makePackageMeta
    model <- PackageMeta.Insert.run packageMeta
    let expected =
          Model.Model
            { Model.key = Witch.from @Int 1,
              Model.value = packageMeta
            }
    IO.liftIO $ model `Hspec.shouldBe` expected

insertPackageMeta :: App.App PackageMeta.Model
insertPackageMeta = do
  x <- makePackageMeta
  PackageMeta.Insert.run x

makePackageMeta :: App.App PackageMeta.PackageMeta
makePackageMeta = do
  version <- do
    x <- Test.arbitrary
    Version.Upsert.run x
  license <- do
    x <- Test.arbitrary
    License.Upsert.run x
  blob <- do
    x <- Test.arbitrary
    Blob.Upsert.run x
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
  Test.arbitraryWith $ \x ->
    x
      { PackageMeta.cabalVersion = Model.key version,
        PackageMeta.license = Model.key license,
        PackageMeta.upload = Model.key upload
      }
