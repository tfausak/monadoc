module Monadoc.Model.Blob where

import qualified Data.ByteString as ByteString
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.Hash as Hash
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model

type Model = Model.Model Blob

type Key = Key.Key Blob

data Blob = Blob
  { contents :: ByteString.ByteString,
    hash :: Hash.Hash,
    size :: Int
  }
  deriving (Eq, Show)

instance Sql.FromRow Blob where
  fromRow =
    Blob
      <$> Sql.field
      <*> Sql.field
      <*> Sql.field

instance Sql.ToRow Blob where
  toRow blob =
    [ Sql.toField $ contents blob,
      Sql.toField $ hash blob,
      Sql.toField $ size blob
    ]

migrations :: [Migration.Migration]
migrations =
  [ Migration.new
      (2022, 3, 16, 0, 0, 0)
      "create table blob \
      \ ( key integer primary key \
      \ , contents blob not null \
      \ , hash blob not null unique \
      \ , size integer not null )"
  ]

new :: ByteString.ByteString -> Blob
new byteString =
  Blob
    { contents = byteString,
      hash = Hash.new byteString,
      size = ByteString.length byteString
    }
