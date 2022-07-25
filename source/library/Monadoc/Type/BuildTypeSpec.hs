module Monadoc.Type.BuildTypeSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.BuildType as BuildType
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.BuildType" $ do
  Hspec.it "can be converted from a string" $ do
    Test.expectTryFrom ("Simple" :: String) BuildType.simple

  Hspec.it "can be converted into a string" $ do
    Test.expectFrom BuildType.simple ("Simple" :: String)

  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlField BuildType.simple $ Sql.SQLText "Simple"

  Hspec.it "can be round-tripped through SQL" $
    QuickCheck.property (Test.propertySqlField @BuildType.BuildType)

  Hspec.it "can be rendered as HTML" $ do
    Test.expectHtml BuildType.simple "Simple"
    Test.expectHtmlRaw BuildType.simple "Simple"
