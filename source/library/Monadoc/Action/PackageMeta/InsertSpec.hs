{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.PackageMeta.InsertSpec where

import qualified Control.Monad.Base as Base
import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Action.Blob.Upsert as Blob.Upsert
import qualified Monadoc.Action.HackageUser.Upsert as HackageUser.Upsert
import qualified Monadoc.Action.License.Upsert as License.Upsert
import qualified Monadoc.Action.Package.Upsert as Package.Upsert
import qualified Monadoc.Action.PackageMeta.Insert as PackageMeta.Insert
import qualified Monadoc.Action.Upload.Upsert as Upload.Upsert
import qualified Monadoc.Action.Version.Upsert as Version.Upsert
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.PackageMeta.Insert" . Hspec.around Test.withConnection $ do
  Hspec.it "inserts a new package meta" . Test.runFake $ do
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
    version <- do
      x <- Test.arbitrary
      Version.Upsert.run x
    upload <- do
      x <- Test.arbitraryWith $ \y ->
        y
          { Upload.blob = Model.key blob,
            Upload.package = Model.key package,
            Upload.uploadedBy = Model.key hackageUser,
            Upload.version = Model.key version
          }
      Upload.Upsert.run x
    packageMeta <- Test.arbitraryWith $ \x ->
      x
        { PackageMeta.cabalVersion = Model.key version,
          PackageMeta.license = Model.key license,
          PackageMeta.upload = Model.key upload
        }
    model <- PackageMeta.Insert.run packageMeta
    let expected =
          Model.Model
            { Model.key = Witch.from @Int 1,
              Model.value = packageMeta
            }
    Base.liftBase $ model `Hspec.shouldBe` expected

insertPackageMeta :: (Base.MonadBase IO m, MonadSql.MonadSql m, Exception.MonadThrow m) => m PackageMeta.Model
insertPackageMeta = do
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
  version <- do
    x <- Test.arbitrary
    Version.Upsert.run x
  upload <- do
    x <- Test.arbitraryWith $ \y ->
      y
        { Upload.blob = Model.key blob,
          Upload.package = Model.key package,
          Upload.uploadedBy = Model.key hackageUser,
          Upload.version = Model.key version
        }
    Upload.Upsert.run x
  x <- Test.arbitraryWith $ \y ->
    y
      { PackageMeta.cabalVersion = Model.key version,
        PackageMeta.license = Model.key license,
        PackageMeta.upload = Model.key upload
      }
  PackageMeta.Insert.run x
