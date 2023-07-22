module Monadoc.Type.Schedule where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text as Text
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Saturn
import qualified Saturn.Unstable.Type.ScheduleSpec as ScheduleSpec
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

newtype Schedule
  = Schedule Saturn.Schedule
  deriving (Eq, Show)

instance Witch.From Saturn.Schedule Schedule

instance Witch.From Schedule Saturn.Schedule

instance Witch.TryFrom Text.Text Schedule where
  tryFrom = Witch.eitherTryFrom $ Bifunctor.bimap (userError . show) Witch.from . Saturn.fromText

instance Witch.From Schedule Text.Text where
  from = Saturn.toText . Witch.from

instance Sql.FromField Schedule where
  fromField field = do
    text <- Sql.fromField field
    either (Sql.returnError Sql.ConversionFailed field . show) pure $
      Witch.tryFrom @Text.Text text

instance Sql.ToField Schedule where
  toField = Sql.toField . Witch.into @Text.Text

instance QuickCheck.Arbitrary Schedule where
  arbitrary = Witch.from <$> ScheduleSpec.arbitrary
