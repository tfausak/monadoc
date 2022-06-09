{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Model.LicenseSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Model.License as License
import qualified Monadoc.Test.Common as Test
import qualified Monadoc.Type.Spdx as Spdx
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Model.License" $ do
  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlRow
      License.License
        { License.spdx = Spdx.none
        }
      [ Sql.SQLText "NONE"
      ]

  Hspec.it "can be round-tripped through SQL" $
    QuickCheck.property (Test.propertySqlRow @License.Model)
