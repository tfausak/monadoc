module Monadoc.Model.Migration where

import qualified Data.Time as Time
import qualified Monadoc.Type.Hash as Hash
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Vendor.SqliteSimple as Sql

type Model = Model.Model Migration

type Key = Key.Key Migration

data Migration = Migration
  { createdAt :: Time.UTCTime,
    hash :: Hash.Hash,
    migratedAt :: Maybe Time.UTCTime
  }
  deriving (Eq, Show)

instance Sql.FromRow Migration where
  fromRow =
    Migration
      <$> Sql.field
      <*> Sql.field
      <*> Sql.field

instance Sql.ToRow Migration where
  toRow migration =
    [ Sql.toField $ createdAt migration,
      Sql.toField $ hash migration,
      Sql.toField $ migratedAt migration
    ]
