{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.RangeSpec where

import qualified Control.Monad.Base as Base
import qualified Monadoc.Action.Range.Insert as Range.Insert
import qualified Monadoc.Model.Range as Range
import qualified Monadoc.Query.Range as Range
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Query.Range" . Hspec.around Test.withConnection $ do
  Hspec.describe "selectByConstraint" $ do
    Hspec.it "returns nothing when there is no range" . Test.runFake $ do
      constraint <- Test.arbitrary
      result <- Range.selectByConstraint constraint
      Base.liftBase $ result `Hspec.shouldBe` Nothing

    Hspec.it "returns a range when one exists" . Test.runFake $ do
      range <- do
        x <- Test.arbitrary
        Range.Insert.run x
      result <- Range.selectByConstraint . Range.constraint $ Model.value range
      Base.liftBase $ result `Hspec.shouldBe` Just range
