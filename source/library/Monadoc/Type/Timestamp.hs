module Monadoc.Type.Timestamp where

import qualified Data.Time as Time
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Lucid
import qualified Witch

newtype Timestamp
  = Timestamp Time.UTCTime
  deriving (Eq, Ord, Show)

instance Witch.From Time.UTCTime Timestamp

instance Witch.From Timestamp Time.UTCTime

instance Sql.FromField Timestamp where
  fromField = fmap (Witch.from @Time.UTCTime) . Sql.fromField

instance Sql.ToField Timestamp where
  toField = Sql.toField . Witch.into @Time.UTCTime

instance Lucid.ToHtml Timestamp where
  toHtml = Lucid.toHtml . Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" . Witch.into @Time.UTCTime
  toHtmlRaw = Lucid.toHtml
