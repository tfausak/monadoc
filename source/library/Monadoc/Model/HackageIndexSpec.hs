module Monadoc.Model.HackageIndexSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Extra.Time as Time
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Test as Test
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Model.HackageIndex" $ do
  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlRow
      HackageIndex.HackageIndex
        { HackageIndex.blob = Witch.from @Int 1,
          HackageIndex.createdAt = Witch.from $ Time.makeUtcTime 2001 2 3 4 5 6.007,
          HackageIndex.processedAt = Just . Witch.from $ Time.makeUtcTime 2002 2 3 4 5 6.007,
          HackageIndex.size = Just 2
        }
      [ Sql.SQLInteger 1,
        Sql.SQLText "2001-02-03 04:05:06.007",
        Sql.SQLText "2002-02-03 04:05:06.007",
        Sql.SQLInteger 2
      ]

  Hspec.it "can be round-tripped through SQL" $
    QuickCheck.property (Test.propertySqlRow @HackageIndex.Model)
