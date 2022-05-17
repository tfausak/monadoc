module Monadoc.Extra.ListSpec where

import qualified Monadoc.Extra.List as List
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Extra.List" $ do
  Hspec.describe "ensureSuffix" $ do
    Hspec.it "works with an empty list" $ do
      List.ensureSuffix '/' "" `Hspec.shouldBe` "/"

    Hspec.it "works with a non-empty list" $ do
      List.ensureSuffix '/' "x" `Hspec.shouldBe` "x/"

    Hspec.it "works with the suffix" $ do
      List.ensureSuffix '/' "x/" `Hspec.shouldBe` "x/"

    Hspec.it "works with just the suffix" $ do
      List.ensureSuffix '/' "/" `Hspec.shouldBe` "/"
