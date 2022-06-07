{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.PackageNameSpec where

import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Distribution.Types.PackageName as Cabal
import qualified Monadoc.Test.Common as Test
import qualified Monadoc.Type.PackageName as PackageName
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.PackageName" $ do
  let packageName = Witch.into @PackageName.PackageName $ Cabal.mkPackageName "example"
  Hspec.it "can be converted from a string" $ do
    Test.expectTryFrom ("example" :: String) packageName

  Hspec.it "can be converted into a string" $ do
    Test.expectFrom packageName ("example" :: String)

  Hspec.it "can be converted from text" $ do
    Test.expectTryFrom ("example" :: Text.Text) packageName

  Hspec.it "can be converted into text" $ do
    Test.expectFrom packageName ("example" :: Text.Text)

  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlField packageName $ Sql.SQLText "example"

  Hspec.it "can be round-tripped through SQL" $
    QuickCheck.property (Test.propertySqlField @PackageName.PackageName)

  Hspec.it "can be rendered as HTML" $ do
    Test.expectHtml packageName "example"
    Test.expectHtmlRaw packageName "example"
