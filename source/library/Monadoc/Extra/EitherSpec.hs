module Monadoc.Extra.EitherSpec where

import qualified Monadoc.Exception.Sick as Sick
import qualified Monadoc.Extra.Either as Either
import qualified Monadoc.Test as Test
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Extra.Either" $ do
  Hspec.describe "hush" $ do
    Hspec.it "turns left into nothing" $ do
      Either.hush (Left 1 :: Either Int Char) `Hspec.shouldBe` Nothing

    Hspec.it "turns right into just" $ do
      Either.hush (Right 'a' :: Either Int Char) `Hspec.shouldBe` Just 'a'

  Hspec.describe "throw" $ do
    Hspec.it "throws left" $ do
      Either.throw (Left Sick.Sick) `Hspec.shouldThrow` Test.exceptionSelector @Sick.Sick

    Hspec.it "returns right" $ do
      Either.throw @Sick.Sick (Right 'a') `Hspec.shouldReturn` 'a'
