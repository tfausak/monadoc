{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.PackageMetaSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.PackageMeta.InsertSpec as PackageMeta.InsertSpec
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Query.PackageMeta as PackageMeta
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Query.PackageMeta" $ do
  Hspec.describe "selectByKey" $ do
    Hspec.it "returns nothing when there is no package meta" . Test.run $ do
      key <- Test.arbitrary
      result <- PackageMeta.selectByKey key
      IO.liftIO $ result `Hspec.shouldBe` Nothing

    Hspec.it "returns a package meta when one exists" . Test.run $ do
      packageMeta <- PackageMeta.InsertSpec.insertPackageMeta
      result <- PackageMeta.selectByKey $ Model.key packageMeta
      IO.liftIO $ result `Hspec.shouldBe` Just packageMeta

  Hspec.describe "selectByUpload" $ do
    Hspec.it "returns nothing when there is no package meta" . Test.run $ do
      upload <- Test.arbitrary
      result <- PackageMeta.selectByUpload upload
      IO.liftIO $ result `Hspec.shouldBe` Nothing

    Hspec.it "returns a package meta when one exists" . Test.run $ do
      packageMeta <- PackageMeta.InsertSpec.insertPackageMeta
      result <- PackageMeta.selectByUpload . PackageMeta.upload $ Model.value packageMeta
      IO.liftIO $ result `Hspec.shouldBe` Just packageMeta
