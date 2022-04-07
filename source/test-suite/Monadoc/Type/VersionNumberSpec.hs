module Monadoc.Type.VersionNumberSpec where

import qualified Data.Version as Version
import qualified Database.SQLite.Simple as Sql
import qualified Distribution.Types.Version as Cabal
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.VersionNumber as VersionNumber
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.VersionNumber" $ do
  let versionNumber = Witch.into @VersionNumber.VersionNumber $ Cabal.mkVersion [1, 2, 3, 4]
  Hspec.it "can be converted from a string" $ do
    Test.expectTryFrom ("1.2.3.4" :: String) versionNumber
  Hspec.it "can be converted into a string" $ do
    Test.expectFrom versionNumber ("1.2.3.4" :: String)
  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlField versionNumber (Sql.SQLText "1.2.3.4")
  Hspec.it "can be rendered as HTML" $ do
    Test.expectHtml versionNumber "1.2.3.4"
    Test.expectHtmlRaw versionNumber "1.2.3.4"
  Hspec.it "can be converted from a base version" $ do
    Test.expectFrom (Version.makeVersion [1, 2, 3, 4]) versionNumber
  Hspec.it "can be converted into a base version" $ do
    Test.expectFrom versionNumber (Version.makeVersion [1, 2, 3, 4])
