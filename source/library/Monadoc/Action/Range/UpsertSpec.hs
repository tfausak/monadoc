{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.Range.UpsertSpec where

import qualified Control.Monad.Base as Base
import qualified Monadoc.Action.Range.Upsert as Range.Upsert
import qualified Monadoc.Test.Common as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Range.Upsert" . Hspec.around Test.withConnection $ do
  Hspec.it "inserts a new range" . Test.runFake $ do
    range <- Test.arbitrary
    actual <- Range.Upsert.run range
    let expected =
          Model.Model
            { Model.key = Witch.from @Int 1,
              Model.value = range
            }
    Base.liftBase $ actual `Hspec.shouldBe` expected

  Hspec.it "updates an existing range" . Test.runFake $ do
    range <- Test.arbitrary
    old <- Range.Upsert.run range
    new <- Range.Upsert.run range
    Base.liftBase $ new `Hspec.shouldBe` old

  Hspec.it "inesrts two ranges" . Test.runFake $ do
    range1 <- Test.arbitrary
    range2 <- Test.arbitrary
    model1 <- Range.Upsert.run range1
    model2 <- Range.Upsert.run range2
    Base.liftBase $ Model.key model1 `Hspec.shouldNotBe` Model.key model2
