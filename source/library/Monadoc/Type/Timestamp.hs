module Monadoc.Type.Timestamp where

import qualified Codec.Archive.Tar.Entry as Tar
import qualified Control.Monad.IO.Class as IO
import qualified Data.Fixed as Fixed
import qualified Data.Hashable as Hashable
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as Time
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Lucid as Html
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

newtype Timestamp
  = Timestamp Time.UTCTime
  deriving (Eq, Ord, Show)

instance Witch.From Time.UTCTime Timestamp

instance Witch.From Timestamp Time.UTCTime

instance Witch.From Timestamp String where
  from =
    Time.formatTime Time.defaultTimeLocale "%0Y-%m-%dT%H:%M:%S%3QZ"
      . Witch.into @Time.UTCTime

instance Sql.FromField Timestamp where
  fromField = fmap (Witch.from @Time.UTCTime) . Sql.fromField

instance Sql.ToField Timestamp where
  toField = Sql.toField . Witch.into @Time.UTCTime

instance Html.ToHtml Timestamp where
  toHtml = Html.toHtml . Witch.into @String
  toHtmlRaw = Html.toHtmlRaw . Witch.into @String

instance QuickCheck.Arbitrary Timestamp where
  arbitrary = Witch.from <$> genUtcTime

instance Witch.From Timestamp Text.Text where
  from = Witch.via @String

instance Hashable.Hashable Timestamp where
  hashWithSalt salt =
    Hashable.hashWithSalt salt
      . Witch.into @Fixed.Pico
      . Witch.into @Time.NominalDiffTime
      . Witch.into @Time.UTCTime

instance Witch.From Tar.EpochTime Timestamp where
  from =
    let pico = 1_000_000_000_000 :: Integer
     in Witch.from
          . Time.posixSecondsToUTCTime
          . Time.secondsToNominalDiffTime
          . Witch.from
          . (*) pico
          . Witch.from

getCurrentTime :: (IO.MonadIO m) => m Timestamp
getCurrentTime = Witch.into @Timestamp <$> IO.liftIO Time.getCurrentTime

genUtcTime :: QuickCheck.Gen Time.UTCTime
genUtcTime = Time.UTCTime <$> genDay <*> genDiffTime

-- The Julian Day Number (JDN) @-678941@ is @0001-01-01@. The JDN @2973483@ is
-- @9999-12-31@. This function generates a day between those two, inclusive.
genDay :: QuickCheck.Gen Time.Day
genDay = Time.ModifiedJulianDay <$> QuickCheck.chooseInteger (-678_941, 2_973_483)

genDiffTime :: QuickCheck.Gen Time.DiffTime
genDiffTime = Time.picosecondsToDiffTime <$> QuickCheck.chooseInteger (0, 24 * 60 * 60 * 1_000_000_000_000)
