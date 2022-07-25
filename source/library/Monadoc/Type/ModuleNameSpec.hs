module Monadoc.Type.ModuleNameSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Distribution.ModuleName as Cabal
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.ModuleName as ModuleName
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.ModuleName" $ do
  let moduleName = Witch.from @Cabal.ModuleName @ModuleName.ModuleName $ Cabal.fromString "Ex.Mod"

  Hspec.it "can be converted from a string" $ do
    Test.expectTryFrom ("Ex.Mod" :: String) moduleName

  Hspec.it "can be converted into a string" $ do
    Test.expectFrom moduleName ("Ex.Mod" :: String)

  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlField moduleName $ Sql.SQLText "Ex.Mod"

  Hspec.it "can be round-tripped through SQL" $
    QuickCheck.property (Test.propertySqlField @ModuleName.ModuleName)

  Hspec.it "can be rendered as HTML" $ do
    Test.expectHtml moduleName "Ex.Mod"
    Test.expectHtmlRaw moduleName "Ex.Mod"
