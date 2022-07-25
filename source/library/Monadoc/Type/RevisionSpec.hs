module Monadoc.Type.RevisionSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Revision as Revision
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Revision" $ do
  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlField Revision.zero $ Sql.SQLInteger 0

  Hspec.it "can be round-tripped through SQL" $
    QuickCheck.property (Test.propertySqlField @Revision.Revision)

  Hspec.it "can be converted into a string" $ do
    Test.expectFrom Revision.zero ("0" :: String)

  Hspec.it "can be rendered as HTML" $ do
    Test.expectHtml Revision.zero "0"
    Test.expectHtmlRaw Revision.zero "0"
