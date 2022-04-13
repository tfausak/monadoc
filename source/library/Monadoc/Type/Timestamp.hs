module Monadoc.Type.Timestamp where

import qualified Control.Monad as Monad
import qualified Data.Fixed as Fixed
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Lucid
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

newtype Timestamp
  = Timestamp Time.UTCTime
  deriving (Eq, Ord, Show)

instance Witch.From Time.UTCTime Timestamp

instance Witch.From Timestamp Time.UTCTime

instance Witch.From Timestamp String where
  from =
    Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
      . Witch.into @Time.UTCTime

instance Sql.FromField Timestamp where
  fromField = fmap (Witch.from @Time.UTCTime) . Sql.fromField

instance Sql.ToField Timestamp where
  toField = Sql.toField . Witch.into @Time.UTCTime

instance Lucid.ToHtml Timestamp where
  toHtml = Lucid.toHtml . Witch.into @String
  toHtmlRaw = Lucid.toHtmlRaw . Witch.into @String

instance QuickCheck.Arbitrary Timestamp where
  arbitrary = Witch.from <$> genUtcTime

instance Witch.From Timestamp Text.Text where
  from = Witch.via @String

genUtcTime :: QuickCheck.Gen Time.UTCTime
genUtcTime = Time.UTCTime <$> genDay <*> fmap Time.timeOfDayToTime genTimeOfDay

genDay :: QuickCheck.Gen Time.Day
genDay = QuickCheck.suchThatMap QuickCheck.arbitrary toDay

toDay :: (Time.Year, Time.MonthOfYear, Time.DayOfMonth) -> Maybe Time.Day
toDay (y, m, d) = do
  -- The sqlite-simple package requires years to have at least 4 digits.
  -- https://github.com/nurpax/sqlite-simple/blob/9190080/Database/SQLite/Simple/Time/Implementation.hs#L50
  Monad.guard $ y >= 1000
  Time.fromGregorianValid y m d

genTimeOfDay :: QuickCheck.Gen Time.TimeOfDay
genTimeOfDay = QuickCheck.suchThatMap QuickCheck.arbitrary toTimeOfDay

toTimeOfDay :: (Int, Int, Fixed.Pico) -> Maybe Time.TimeOfDay
toTimeOfDay (h, m, s) = Time.makeTimeOfDayValid h m s

fromTimeOfDay :: Time.TimeOfDay -> (Int, Int, Fixed.Pico)
fromTimeOfDay t = (Time.todHour t, Time.todMin t, Time.todSec t)
