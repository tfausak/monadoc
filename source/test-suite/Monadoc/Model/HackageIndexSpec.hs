module Monadoc.Model.HackageIndexSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Extra.Time as Time
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Test as Test
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Model.HackageIndex" $ do
  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlRow
      HackageIndex.HackageIndex
        { HackageIndex.contents = "example",
          HackageIndex.processedAt = Just . Witch.from $ Time.makeUtcTime 2001 2 3 4 5 6.007,
          HackageIndex.size = 7
        }
      [ Sql.SQLBlob "example",
        Sql.SQLText "2001-02-03 04:05:06.007",
        Sql.SQLInteger 7
      ]
