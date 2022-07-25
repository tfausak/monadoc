module Monadoc.Action.PackageMeta.UpsertSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.PackageMeta.InsertSpec as PackageMeta.InsertSpec
import qualified Monadoc.Action.PackageMeta.Upsert as PackageMeta.Upsert
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Hash as Hash
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.PackageMeta.Upsert" $ do
  Hspec.it "inserts a new package meta" . Test.run $ do
    packageMeta <- PackageMeta.InsertSpec.makePackageMeta
    actual <- PackageMeta.Upsert.run packageMeta
    let expected =
          Model.Model
            { Model.key = Witch.from @Int 1,
              Model.value = packageMeta
            }
    IO.liftIO $ actual `Hspec.shouldBe` expected

  Hspec.it "inserts two package metas" . Test.run $ do
    packageMeta1 <- PackageMeta.InsertSpec.makePackageMeta
    packageMeta2 <- PackageMeta.InsertSpec.makePackageMeta
    model1 <- PackageMeta.Upsert.run packageMeta1
    model2 <- PackageMeta.Upsert.run packageMeta2
    IO.liftIO $ Model.key model1 `Hspec.shouldNotBe` Model.key model2

  Hspec.it "updates an existing package meta" . Test.run $ do
    model <- PackageMeta.InsertSpec.insertPackageMeta
    let packageMeta = (Model.value model) {PackageMeta.hash = Hash.new "updated"}
    result <- PackageMeta.Upsert.run packageMeta
    IO.liftIO $ result `Hspec.shouldBe` model {Model.value = packageMeta}
