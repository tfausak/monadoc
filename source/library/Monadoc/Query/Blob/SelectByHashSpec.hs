{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.Blob.SelectByHashSpec where

import qualified Control.Monad.Base as Base
import qualified Monadoc.Action.Blob.Insert as Blob.Insert
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Query.Blob.SelectByHash as Blob.SelectByHash
import qualified Monadoc.Test.Common as Test
import qualified Monadoc.Type.Hash as Hash
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Query.Blob.SelectByHash" . Hspec.around Test.withConnection $ do
  Hspec.it "returns nothing when the blob doesn't exist" . Test.runFake $ do
    result <- Blob.SelectByHash.run $ Hash.new ""
    Base.liftBase $ result `Hspec.shouldBe` Nothing

  Hspec.it "returns just when the blob exists" . Test.runFake $ do
    let blob = Blob.new ""
    model <- Blob.Insert.run blob
    result <- Blob.SelectByHash.run $ Blob.hash blob
    Base.liftBase $ result `Hspec.shouldBe` Just model
