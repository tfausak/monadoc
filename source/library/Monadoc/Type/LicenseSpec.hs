{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.LicenseSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Distribution.SPDX as Cabal
import qualified Monadoc.Test.Common as Test
import qualified Monadoc.Type.License as License
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.License" $ do
  let license = Witch.into @License.License . Cabal.License $ Cabal.ELicense (Cabal.ELicenseId Cabal.MIT) Nothing
  Hspec.it "can be converted from a string" $ do
    Test.expectTryFrom ("MIT" :: String) license

  Hspec.it "can be converted into a string" $ do
    Test.expectFrom license ("MIT" :: String)

  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlField license (Sql.SQLText "MIT")

  Hspec.it "can be round-tripped through SQL" $
    QuickCheck.property (Test.propertySqlField @License.License)
