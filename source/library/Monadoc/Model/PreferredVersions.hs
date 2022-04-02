module Monadoc.Model.PreferredVersions where

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.VersionRange as VersionRange

type Model = Model.Model PreferredVersions

type Key = Key.Key PreferredVersions

data PreferredVersions = PreferredVersions
  { package :: Package.Key,
    range :: VersionRange.VersionRange
  }
  deriving (Eq, Show)

instance Sql.FromRow PreferredVersions where
  fromRow =
    PreferredVersions
      <$> Sql.field
      <*> Sql.field

instance Sql.ToRow PreferredVersions where
  toRow preferredVersions =
    [ Sql.toField $ package preferredVersions,
      Sql.toField $ range preferredVersions
    ]

migrations :: [Migration.Migration]
migrations =
  [ Migration.new
      (2022, 3, 19, 1, 0, 0)
      "create table preferredVersions \
      \ ( key integer primary key \
      \ , package integer not null unique references package \
      \ , range text not null )"
  ]
