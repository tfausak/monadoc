{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.Schedule where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified System.Cron as Cron
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

newtype Schedule
  = Schedule Cron.CronSchedule
  deriving (Eq, Show)

instance Witch.From Cron.CronSchedule Schedule

instance Witch.From Schedule Cron.CronSchedule

instance Witch.TryFrom Text.Text Schedule where
  tryFrom = Witch.eitherTryFrom $ Bifunctor.bimap userError Witch.from . Cron.parseCronSchedule

instance Witch.From Schedule Text.Text where
  from = Cron.serializeCronSchedule . Witch.from

instance Sql.FromField Schedule where
  fromField field = do
    text <- Sql.fromField field
    either (Sql.returnError Sql.ConversionFailed field . show) pure $
      Witch.tryFrom @Text.Text text

instance Sql.ToField Schedule where
  toField = Sql.toField . Witch.into @Text.Text

instance QuickCheck.Arbitrary Schedule where
  arbitrary = Witch.from <$> genCronSchedule

genCronSchedule :: QuickCheck.Gen Cron.CronSchedule
genCronSchedule =
  Cron.CronSchedule
    <$> genMinuteSpec
    <*> genHourSpec
    <*> genDayOfMonthSpec
    <*> genMonthSpec
    <*> genDayOfWeekSpec

genMinuteSpec :: QuickCheck.Gen Cron.MinuteSpec
genMinuteSpec = QuickCheck.suchThatMap genCronField Cron.mkMinuteSpec

genHourSpec :: QuickCheck.Gen Cron.HourSpec
genHourSpec = QuickCheck.suchThatMap genCronField Cron.mkHourSpec

genDayOfMonthSpec :: QuickCheck.Gen Cron.DayOfMonthSpec
genDayOfMonthSpec = QuickCheck.suchThatMap genCronField Cron.mkDayOfMonthSpec

genMonthSpec :: QuickCheck.Gen Cron.MonthSpec
genMonthSpec = QuickCheck.suchThatMap genCronField Cron.mkMonthSpec

genDayOfWeekSpec :: QuickCheck.Gen Cron.DayOfWeekSpec
genDayOfWeekSpec = QuickCheck.suchThatMap genCronField Cron.mkDayOfWeekSpec

genCronField :: QuickCheck.Gen Cron.CronField
genCronField =
  QuickCheck.oneof
    [ Cron.Field <$> genBaseField,
      Cron.ListField <$> genListField,
      Cron.StepField' <$> genStepField
    ]

genBaseField :: QuickCheck.Gen Cron.BaseField
genBaseField =
  QuickCheck.oneof
    [ pure Cron.Star,
      Cron.SpecificField' <$> genSpecificField,
      Cron.RangeField' <$> genRangeField
    ]

genSpecificField :: QuickCheck.Gen Cron.SpecificField
genSpecificField = QuickCheck.suchThatMap QuickCheck.arbitrary Cron.mkSpecificField

genRangeField :: QuickCheck.Gen Cron.RangeField
genRangeField = QuickCheck.suchThatMap QuickCheck.arbitrary $ uncurry Cron.mkRangeField

genListField :: QuickCheck.Gen (NonEmpty.NonEmpty Cron.BaseField)
genListField = genNonEmpty genBaseField

genNonEmpty :: QuickCheck.Gen a -> QuickCheck.Gen (NonEmpty.NonEmpty a)
genNonEmpty g = (NonEmpty.:|) <$> g <*> QuickCheck.listOf g

genStepField :: QuickCheck.Gen Cron.StepField
genStepField = QuickCheck.suchThatMap ((,) <$> genBaseField <*> QuickCheck.arbitrary) $ uncurry Cron.mkStepField
