{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.BlobSpec where

import qualified Control.Monad.Base as Base
import qualified Monadoc.Action.Blob.Insert as Blob.Insert
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Query.Blob as Blob
import qualified Monadoc.Test.Common as Test
import qualified Monadoc.Type.Hash as Hash
import qualified Test.Hspec as Hspec

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
