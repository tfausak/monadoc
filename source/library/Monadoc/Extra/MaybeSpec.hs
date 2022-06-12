module Monadoc.Extra.MaybeSpec where

import qualified Monadoc.Extra.Maybe as Maybe
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Extra.Maybe" $ do
  Hspec.describe "note" $ do
    Hspec.it "turns nothing into left" $ do
      Maybe.note 'a' (Nothing :: Maybe Int) `Hspec.shouldBe` Left 'a'

    Hspec.it "turns just into right" $ do
      Maybe.note 'a' (Just 1 :: Maybe Int) `Hspec.shouldBe` Right 1
