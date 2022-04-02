module Monadoc.Model.HackageIndex where

import qualified Data.ByteString as ByteString
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Timestamp as Timestamp

type Model = Model.Model HackageIndex

type Key = Key.Key HackageIndex

data HackageIndex = HackageIndex
  { contents :: ByteString.ByteString,
    processedAt :: Maybe Timestamp.Timestamp,
    size :: Int
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
    [ Sql.toField $ contents hackageIndex,
      Sql.toField $ processedAt hackageIndex,
      Sql.toField $ size hackageIndex
    ]

migrations :: [Migration.Migration]
migrations =
  [ Migration.new
      (2022, 3, 12, 0, 0, 0)
      "create table hackageIndex \
      \ ( key integer primary key \
      \ , contents blob not null \
      \ , processedAt text \
      \ , size integer not null )"
  ]
