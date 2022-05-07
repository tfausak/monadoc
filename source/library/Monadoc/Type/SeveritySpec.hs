module Monadoc.Type.SeveritySpec where

import qualified Data.List as List
import qualified Monadoc.Test.Common as Test
import qualified Monadoc.Type.Severity as Severity
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Severity" $ do
  Hspec.it "has the right order" $ do
    let severities = [Severity.Debug, Severity.Info, Severity.Warn, Severity.Error]
    List.sort severities `Hspec.shouldBe` severities

  Hspec.it "can be converted into a string" $ do
    Test.expectFrom Severity.Debug ("debug" :: String)
    Test.expectFrom Severity.Info ("info" :: String)
    Test.expectFrom Severity.Warn ("warn" :: String)
    Test.expectFrom Severity.Error ("error" :: String)
