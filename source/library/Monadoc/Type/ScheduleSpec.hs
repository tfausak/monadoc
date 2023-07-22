module Monadoc.Type.ScheduleSpec where

import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Schedule as Schedule
import qualified Saturn
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Schedule" $ do
  let schedule = Witch.into @Schedule.Schedule Saturn.everyMinute

  Hspec.it "can be converted from text" $ do
    Test.expectTryFrom ("* * * * *" :: Text.Text) schedule

  Hspec.it "can be converted into text" $ do
    Test.expectFrom schedule ("* * * * *" :: Text.Text)

  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlField schedule (Sql.SQLText "* * * * *")

  Hspec.it "can be round-tripped through SQL" $
    QuickCheck.property (Test.propertySqlField @Schedule.Schedule)
