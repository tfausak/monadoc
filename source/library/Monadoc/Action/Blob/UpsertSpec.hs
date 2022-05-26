{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.Blob.UpsertSpec where

import qualified Control.Monad.Base as Base
import qualified Monadoc.Action.Blob.Upsert as Blob.Upsert
import qualified Monadoc.Test.Common as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Blob.Upsert" . Hspec.around Test.withConnection $ do
  Hspec.it "inserts a new blob" . Test.runFake $ do
    blob <- Test.arbitrary
    model <- Blob.Upsert.run blob
    Base.liftBase $
      model
        `Hspec.shouldBe` Model.Model
          { Model.key = Witch.from @Int 1,
            Model.value = blob
          }

  Hspec.it "updates an existing blob" . Test.runFake $ do
    blob <- Test.arbitrary
    old <- Blob.Upsert.run blob
    new <- Blob.Upsert.run blob
    Base.liftBase $ new `Hspec.shouldBe` old

  Hspec.it "inserts two blobs" . Test.runFake $ do
    blob1 <- do
      x <- Test.arbitrary
      Blob.Upsert.run x
    blob2 <- do
      x <- Test.arbitrary
      Blob.Upsert.run x
    Base.liftBase $ Model.key blob1 `Hspec.shouldNotBe` Model.key blob2
