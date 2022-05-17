module Monadoc.Extra.EitherSpec where

import qualified Monadoc.Extra.Either as Either
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Extra.Either" $ do
  Hspec.describe "hush" $ do
    Hspec.it "turns left into nothing" $ do
      Either.hush (Left 1 :: Either Int Char) `Hspec.shouldBe` Nothing

    Hspec.it "turns right into just" $ do
      Either.hush (Right 'a' :: Either Int Char) `Hspec.shouldBe` Just 'a'

  Hspec.describe "note" $ do
    Hspec.it "turns nothing into left" $ do
      Either.note 'a' (Nothing :: Maybe Int) `Hspec.shouldBe` Left 'a'

    Hspec.it "turns just into right" $ do
      Either.note 'a' (Just 1 :: Maybe Int) `Hspec.shouldBe` Right 1
