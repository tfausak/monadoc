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
    version :: Version.Key,
    isPreferred :: Bool,
    isLatest :: Bool
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
      <*> Sql.field
      <*> Sql.field

instance Sql.ToRow Upload where
  toRow upload =
    [ Sql.toField upload.blob,
      Sql.toField upload.package,
      Sql.toField upload.revision,
      Sql.toField upload.uploadedAt,
      Sql.toField upload.uploadedBy,
      Sql.toField upload.version,
      Sql.toField upload.isPreferred,
      Sql.toField upload.isLatest
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
      <*> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary

migrations :: [Migration.Migration]
migrations =
  [ Migration.new
      (2022, 1, 10, 0, 0, 0)
      "create table upload \
      \ ( key integer primary key \
      \ , blob integer not null references blob \
      \ , package integer not null references package \
      \ , revision integer not null \
      \ , uploadedAt text not null \
      \ , uploadedBy integer not null references hackageUser \
      \ , version integer not null references version \
      \ , unique ( package, version, revision ) )",
    Migration.new
      (2022, 1, 11, 0, 0, 0)
      "alter table upload add column isPreferred integer default true",
    Migration.new
      (2022, 1, 12, 0, 0, 0)
      "alter table upload add column isLatest integer default false",
    Migration.new
      (2022, 5, 23, 6, 53, 0)
      "create index upload_package on upload ( package )",
    Migration.new
      (2022, 5, 23, 6, 56, 0)
      "create index upload_version on upload ( version )",
    Migration.new
      (2022, 5, 23, 6, 57, 0)
      "create index upload_revision on upload ( revision )",
    Migration.new
      (2022, 5, 23, 6, 58, 0)
      "create index upload_uploadedAt on upload ( uploadedAt )",
    Migration.new
      (2022, 5, 23, 6, 59, 0)
      "create index upload_isLatest on upload ( isLatest )",
    Migration.new
      (2022, 5, 23, 7, 0, 0)
      "create index upload_uploadedBy on upload ( uploadedBy )"
  ]
