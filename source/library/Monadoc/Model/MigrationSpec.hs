{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Model.MigrationSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Extra.Time as Time
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Test.Common as Test
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Model.Migration" $ do
  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlRow
      Migration.Migration
        { Migration.createdAt = Witch.from $ Time.makeUtcTime 2001 2 3 4 5 6.007,
          Migration.query = "select 1"
        }
      [ Sql.SQLText "2001-02-03 04:05:06.007",
        Sql.SQLText "select 1"
      ]

  Hspec.it "can be round-tripped through SQL" $
    QuickCheck.property (Test.propertySqlRow @Migration.Model)
