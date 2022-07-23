{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.ModuleTypeSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.ModuleType as ModuleType
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.ModuleType" $ do
  Hspec.it "can be converted from a string" $ do
    Test.expectTryFrom ("autogen" :: String) ModuleType.Autogen
    Test.expectTryFrom ("exposed" :: String) ModuleType.Exposed
    Test.expectTryFrom ("other" :: String) ModuleType.Other
    Test.expectTryFrom ("virtual" :: String) ModuleType.Virtual

  Hspec.it "can be converted into a string" $ do
    Test.expectFrom ModuleType.Autogen ("autogen" :: String)
    Test.expectFrom ModuleType.Exposed ("exposed" :: String)
    Test.expectFrom ModuleType.Other ("other" :: String)
    Test.expectFrom ModuleType.Virtual ("virtual" :: String)

  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlField ModuleType.Autogen $ Sql.SQLText "autogen"
    Test.expectSqlField ModuleType.Exposed $ Sql.SQLText "exposed"
    Test.expectSqlField ModuleType.Other $ Sql.SQLText "other"
    Test.expectSqlField ModuleType.Virtual $ Sql.SQLText "virtual"

  Hspec.it "can be round-tripped through SQL" $
    QuickCheck.property (Test.propertySqlField @ModuleType.ModuleType)

  Hspec.it "can be rendered as HTML" $ do
    Test.expectHtml ModuleType.Virtual "virtual"
    Test.expectHtmlRaw ModuleType.Virtual "virtual"
