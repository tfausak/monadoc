{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Type.ReversionSpec where

import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Reversion as Reversion
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.VersionNumber as VersionNumber
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Reversion" $ do
  Hspec.it "can be converted into a string" $ do
    Test.expectFrom (Reversion.Reversion VersionNumber.zero Nothing) ("0" :: String)
    Test.expectFrom (Reversion.Reversion VersionNumber.zero (Just Revision.zero)) ("0+0" :: String)

  Hspec.it "can be rendered as HTML" $ do
    Test.expectHtml (Reversion.Reversion VersionNumber.zero Nothing) "0"
    Test.expectHtmlRaw (Reversion.Reversion VersionNumber.zero (Just Revision.zero)) "0+0"

  Hspec.it "can be converted from a string" $ do
    Test.expectTryFrom ("0" :: String) (Reversion.Reversion VersionNumber.zero Nothing)
    Test.expectTryFrom ("0+0" :: String) (Reversion.Reversion VersionNumber.zero (Just Revision.zero))
