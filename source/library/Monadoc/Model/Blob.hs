module Monadoc.Model.Blob where

import qualified Data.ByteString as ByteString
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.Hash as Hash
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Test.QuickCheck as QuickCheck

type Model = Model.Model Blob

type Key = Key.Key Blob

data Blob = Blob
  { size :: Int,
    hash :: Hash.Hash,
    contents :: ByteString.ByteString
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
    [ Sql.toField blob.size,
      Sql.toField blob.hash,
      Sql.toField blob.contents
    ]

instance QuickCheck.Arbitrary Blob where
  arbitrary = new . ByteString.pack <$> QuickCheck.arbitrary

migrations :: [Migration.Migration]
migrations =
  [ Migration.new
      (2022, 1, 2, 0, 0, 0)
      "create table blob \
      \ ( key integer primary key \
      \ , size integer not null \
      \ , hash blob not null unique \
      \ , contents blob not null )"
  ]

new :: ByteString.ByteString -> Blob
new c =
  Blob
    { contents = c,
      hash = Hash.new c,
      size = ByteString.length c
    }
