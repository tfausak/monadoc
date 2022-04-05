module Monadoc.Type.PortSpec where

import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Port as Port
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Port" $ do
  Hspec.it "can be converted from an int" $ do
    Test.expectFrom (80 :: Int) (Port.Port 80)
  Hspec.it "can be converted into an int" $ do
    Test.expectFrom (Port.Port 80) (80 :: Int)
  Hspec.it "can be converted from a string" $ do
    Test.expectTryFrom ("80" :: String) (Port.Port 80)
