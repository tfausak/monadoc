module Monadoc.Extra.EitherSpec where

import qualified Monadoc.Extra.Either as Extra
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Extra.Either" $ do
  Hspec.describe "hush" $ do
    Hspec.it "turns left into nothing" $ do
      Extra.hush (Left () :: Either () ()) `Hspec.shouldBe` Nothing

    Hspec.it "turns right into just" $ do
      Extra.hush (Right () :: Either () ()) `Hspec.shouldBe` Just ()
