module Monadoc.Model.Release where

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

type Model = Model.Model Release

type Key = Key.Key Release

data Release = Release
  { blob :: Blob.Key,
    package :: Package.Key,
    revision :: Revision.Revision,
    uploadedAt :: Timestamp.Timestamp,
    uploadedBy :: HackageUser.Key,
    version :: Version.Key
  }
  deriving (Eq, Show)

instance Sql.FromRow Release where
  fromRow =
    Release
      <$> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field

instance Sql.ToRow Release where
  toRow release =
    [ Sql.toField $ blob release,
      Sql.toField $ package release,
      Sql.toField $ revision release,
      Sql.toField $ uploadedAt release,
      Sql.toField $ uploadedBy release,
      Sql.toField $ version release
    ]

migrations :: [Migration.Migration]
migrations =
  [ Migration.new
      (2022, 3, 19, 0, 0, 0)
      "create table release \
      \ ( key integer primary key \
      \ , blob integer not null references blob \
      \ , package integer not null references package \
      \ , revision integer not null \
      \ , uploadedAt text not null \
      \ , uploadedBy integer not null references hackageUser \
      \ , version integer not null references version \
      \ , unique ( package, version, revision ) )"
  ]
