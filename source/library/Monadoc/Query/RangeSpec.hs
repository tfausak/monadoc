{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.RangeSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.Range.Insert as Range.Insert
import qualified Monadoc.Model.Range as Range
import qualified Monadoc.Query.Range as Range
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Query.Range" $ do
  Hspec.describe "selectByConstraint" $ do
    Hspec.it "returns nothing when there is no range" . Test.run $ do
      constraint <- Test.arbitrary
      result <- Range.selectByConstraint constraint
      IO.liftIO $ result `Hspec.shouldBe` Nothing

    Hspec.it "returns a range when one exists" . Test.run $ do
      range <- do
        x <- Test.arbitrary
        Range.Insert.run x
      result <- Range.selectByConstraint . Range.constraint $ Model.value range
      IO.liftIO $ result `Hspec.shouldBe` Just range
