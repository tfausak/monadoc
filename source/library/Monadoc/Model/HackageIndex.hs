module Monadoc.Model.HackageIndex where

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Test.QuickCheck as QuickCheck

type Model = Model.Model HackageIndex

type Key = Key.Key HackageIndex

data HackageIndex = HackageIndex
  { blob :: Blob.Key,
    createdAt :: Timestamp.Timestamp,
    processedAt :: Maybe Timestamp.Timestamp
  }
  deriving (Eq, Show)

instance Sql.FromRow HackageIndex where
  fromRow =
    HackageIndex
      <$> Sql.field
      <*> Sql.field
      <*> Sql.field

instance Sql.ToRow HackageIndex where
  toRow hackageIndex =
    [ Sql.toField $ blob hackageIndex,
      Sql.toField $ createdAt hackageIndex,
      Sql.toField $ processedAt hackageIndex
    ]

instance QuickCheck.Arbitrary HackageIndex where
  arbitrary =
    HackageIndex
      <$> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary

migrations :: [Migration.Migration]
migrations =
  [ Migration.new
      (2022, 1, 4, 0, 0, 0)
      "create table hackageIndex \
      \ ( key integer primary key \
      \ , createdAt text not null \
      \ , processedAt text \
      \ , size integer not null \
      \ , updatedAt text \
      \ , contents blob not null )",
    Migration.new
      (2022, 5, 22, 19, 31, 0)
      "drop table hackageIndex",
    Migration.new
      (2022, 5, 22, 19, 32, 0)
      "create table hackageIndex \
      \ ( key integer primary key \
      \ , blob integer not null references blob \
      \ , createdAt text not null \
      \ , processedAt text )",
    Migration.new
      (2022, 5, 23, 6, 51, 0)
      "create index hackageIndex_processedAt on hackageIndex ( processedAt )",
    Migration.new
      (2022, 5, 23, 6, 52, 0)
      "create index hackageIndex_createdAt on hackageIndex ( createdAt )"
  ]
