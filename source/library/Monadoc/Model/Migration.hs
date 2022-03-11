module Monadoc.Model.Migration where

import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import Monadoc.Orphanage ()
import qualified Monadoc.Type.Hash as Hash
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Witch

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

createTable :: Sql.Query
createTable =
  Witch.from
    "create table if not exists migration \
    \ ( key integer not null primary key \
    \ , createdAt text not null \
    \ , hash text not null \
    \ , migratedAt text )"
