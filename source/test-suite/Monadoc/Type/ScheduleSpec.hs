module Monadoc.Type.ScheduleSpec where

import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Schedule as Schedule
import qualified System.Cron as Cron
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Schedule" $ do
  Hspec.it "can be converted from text" $ do
    Test.expectTryFrom ("* * * * *" :: Text.Text) (Witch.into @Schedule.Schedule Cron.everyMinute)

  Hspec.it "can be converted into text" $ do
    Test.expectFrom (Witch.into @Schedule.Schedule Cron.everyMinute) ("* * * * *" :: Text.Text)

  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlField (Witch.into @Schedule.Schedule Cron.everyMinute) (Sql.SQLText "* * * * *")
