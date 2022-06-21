{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.ComponentNameSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Distribution.Types.UnqualComponentName as Cabal
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.ComponentName as ComponentName
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.ComponentName" $ do
  let componentName = Witch.into @ComponentName.ComponentName $ Cabal.mkUnqualComponentName "example"
  Hspec.it "can be converted from a string" $ do
    Test.expectTryFrom ("example" :: String) componentName

  Hspec.it "can be converted into a string" $ do
    Test.expectFrom componentName ("example" :: String)

  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlField componentName $ Sql.SQLText "example"

  Hspec.it "can be round-tripped through SQL" $
    QuickCheck.property (Test.propertySqlField @ComponentName.ComponentName)

  Hspec.it "can be rendered as HTML" $ do
    Test.expectHtml componentName "example"
    Test.expectHtmlRaw componentName "example"
