module Monadoc.Model.Upload where

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Test.QuickCheck as QuickCheck

type Model = Model.Model Upload

type Key = Key.Key Upload

data Upload = Upload
  { blob :: Blob.Key,
    package :: Package.Key,
    revision :: Revision.Revision,
    uploadedAt :: Timestamp.Timestamp,
    uploadedBy :: HackageUser.Key,
    version :: Version.Key
  }
  deriving (Eq, Show)

instance Sql.FromRow Upload where
  fromRow =
    Upload
      <$> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field

instance Sql.ToRow Upload where
  toRow upload =
    [ Sql.toField $ blob upload,
      Sql.toField $ package upload,
      Sql.toField $ revision upload,
      Sql.toField $ uploadedAt upload,
      Sql.toField $ uploadedBy upload,
      Sql.toField $ version upload
    ]

instance QuickCheck.Arbitrary Upload where
  arbitrary =
    Upload
      <$> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary

migrations :: [Migration.Migration]
migrations =
  [ Migration.new
      (2022, 4, 6, 0, 0, 0)
      "create table upload \
      \ ( key integer primary key \
      \ , blob integer not null references blob \
      \ , package integer not null references package \
      \ , revision integer not null \
      \ , uploadedAt text not null \
      \ , uploadedBy integer not null references hackageUser \
      \ , version integer not null references version \
      \ , unique ( package, version, revision ) )"
  ]
