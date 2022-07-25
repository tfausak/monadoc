module Monadoc.Model.CronEntrySpec where

import qualified Data.UUID as Uuid
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Extra.Time as Time
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Task as Task
import qualified System.Cron as Cron
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Model.CronEntry" $ do
  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlRow
      CronEntry.CronEntry
        { CronEntry.guid = Just $ Witch.from Uuid.nil,
          CronEntry.runAt = Witch.from Time.epoch,
          CronEntry.schedule = Witch.from Cron.everyMinute,
          CronEntry.task = Task.Vacuum
        }
      [ Sql.SQLBlob "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00",
        Sql.SQLText "1970-01-01 00:00:00",
        Sql.SQLText "* * * * *",
        Sql.SQLBlob "{\"tag\":\"Vacuum\"}"
      ]

  Hspec.it "can be round-tripped through SQL" $
    QuickCheck.property (Test.propertySqlRow @CronEntry.Model)
