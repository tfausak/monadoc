{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Query.BlobSpec where

import qualified Control.Monad.Base as Base
import qualified Monadoc.Action.Blob.Insert as Blob.Insert
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Query.Blob as Blob
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Hash as Hash
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Query.Blob" . Hspec.around Test.withConnection $ do
  Hspec.describe "selectByHash" $ do
    Hspec.it "returns nothing when the blob doesn't exist" . Test.runFake $ do
      result <- Blob.selectByHash $ Hash.new ""
      Base.liftBase $ result `Hspec.shouldBe` Nothing

    Hspec.it "returns just when the blob exists" . Test.runFake $ do
      blob <- Test.arbitrary
      model <- Blob.Insert.run blob
      result <- Blob.selectByHash $ Blob.hash blob
      Base.liftBase $ result `Hspec.shouldBe` Just model

  Hspec.describe "selectByKey" $ do
    Hspec.it "returns nothing when the blob doesn't exist" . Test.runFake $ do
      result <- Blob.selectByKey $ Witch.from @Int 0
      Base.liftBase $ result `Hspec.shouldBe` Nothing

    Hspec.it "returns just when the blob exists" . Test.runFake $ do
      blob <- Test.arbitrary
      model <- Blob.Insert.run blob
      result <- Blob.selectByKey $ Model.key model
      Base.liftBase $ result `Hspec.shouldBe` Just model
