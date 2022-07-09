{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.LicenseSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.License.Insert as License.Insert
import qualified Monadoc.Model.License as License
import qualified Monadoc.Query.License as License
import qualified Monadoc.Test as Test
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Query.License" $ do
  Hspec.describe "selectBySpdx" $ do
    Hspec.it "returns nothing when there is no license" . Test.run $ do
      spdx <- Test.arbitrary
      result <- License.selectBySpdx spdx
      IO.liftIO $ result `Hspec.shouldBe` Nothing

    Hspec.it "returns a license when one exists" . Test.run $ do
      license <- Test.arbitrary
      model <- License.Insert.run license
      result <- License.selectBySpdx $ License.spdx license
      IO.liftIO $ result `Hspec.shouldBe` Just model
