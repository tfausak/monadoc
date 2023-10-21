module Monadoc.Action.PackageMeta.UpsertSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.PackageMeta.Upsert as PackageMeta.Upsert
import qualified Monadoc.Factory as Factory
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Hash as Hash
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.PackageMeta.Upsert" $ do
  Hspec.it "inserts a new package meta" . Test.run $ do
    packageMeta <- makePackageMeta
    actual <- PackageMeta.Upsert.run packageMeta
    let expected =
          Model.Model
            { Model.key = Witch.from @Int 1,
              Model.value = packageMeta
            }
    IO.liftIO $ actual `Hspec.shouldBe` expected

  Hspec.it "inserts two package metas" . Test.run $ do
    packageMeta1 <- makePackageMeta
    packageMeta2 <- makePackageMeta
    model1 <- PackageMeta.Upsert.run packageMeta1
    model2 <- PackageMeta.Upsert.run packageMeta2
    IO.liftIO $ model1.key `Hspec.shouldNotBe` model2.key

  Hspec.it "updates an existing package meta" . Test.run $ do
    model <- Factory.newPackageMeta
    let packageMeta = model.value {PackageMeta.hash = Hash.new "updated"}
    result <- PackageMeta.Upsert.run packageMeta
    IO.liftIO $ result `Hspec.shouldBe` model {Model.value = packageMeta}

makePackageMeta :: App.App PackageMeta.PackageMeta
makePackageMeta = do
  version <- Factory.newVersion
  license <- Factory.newLicense
  upload <- Factory.newUpload
  Test.arbitraryWith $ \packageMeta ->
    packageMeta
      { PackageMeta.cabalVersion = version.key,
        PackageMeta.license = license.key,
        PackageMeta.upload = upload.key
      }
