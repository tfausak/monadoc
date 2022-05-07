{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Model.HackageIndexSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Extra.Time as Time
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Test.Common as Test
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Model.HackageIndex" $ do
  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlRow
      HackageIndex.HackageIndex
        { HackageIndex.contents = "example",
          HackageIndex.createdAt = Witch.from $ Time.makeUtcTime 2001 2 3 4 5 6.007,
          HackageIndex.processedAt = Just . Witch.from $ Time.makeUtcTime 2002 2 3 4 5 6.007,
          HackageIndex.size = 7,
          HackageIndex.updatedAt = Just . Witch.from $ Time.makeUtcTime 2003 2 3 4 5 6.007
        }
      [ Sql.SQLText "2001-02-03 04:05:06.007",
        Sql.SQLText "2002-02-03 04:05:06.007",
        Sql.SQLInteger 7,
        Sql.SQLText "2003-02-03 04:05:06.007",
        Sql.SQLBlob "example"
      ]
