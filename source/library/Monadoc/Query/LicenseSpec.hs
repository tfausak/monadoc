{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.LicenseSpec where

import qualified Control.Monad.Base as Base
import qualified Monadoc.Action.License.Insert as License.Insert
import qualified Monadoc.Model.License as License
import qualified Monadoc.Query.License as License
import qualified Monadoc.Test as Test
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Query.License" . Hspec.around Test.withConnection $ do
  Hspec.describe "selectBySpdx" $ do
    Hspec.it "returns nothing when there is no license" . Test.runFake $ do
      spdx <- Test.arbitrary
      result <- License.selectBySpdx spdx
      Base.liftBase $ result `Hspec.shouldBe` Nothing

    Hspec.it "returns a license when one exists" . Test.runFake $ do
      license <- Test.arbitrary
      model <- License.Insert.run license
      result <- License.selectBySpdx $ License.spdx license
      Base.liftBase $ result `Hspec.shouldBe` Just model
