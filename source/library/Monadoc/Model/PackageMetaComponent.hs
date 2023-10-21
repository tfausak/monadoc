module Monadoc.Model.PackageMetaComponent where

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Test.QuickCheck as QuickCheck

type Model = Model.Model PackageMetaComponent

type Key = Key.Key PackageMetaComponent

data PackageMetaComponent = PackageMetaComponent
  { packageMeta :: PackageMeta.Key,
    component :: Component.Key
  }
  deriving (Eq, Show)

instance Sql.FromRow PackageMetaComponent where
  fromRow =
    PackageMetaComponent
      <$> Sql.field
      <*> Sql.field

instance Sql.ToRow PackageMetaComponent where
  toRow cronEntry =
    [ Sql.toField cronEntry.packageMeta,
      Sql.toField cronEntry.component
    ]

instance QuickCheck.Arbitrary PackageMetaComponent where
  arbitrary =
    PackageMetaComponent
      <$> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary

migrations :: [Migration.Migration]
migrations =
  [ Migration.new
      (2022, 6, 20, 7, 46, 0)
      "create table packageMetaComponent \
      \ ( key integer primary key \
      \ , packageMeta integer not null references packageMeta \
      \ , component integer not null references component \
      \ , unique (packageMeta, component) )"
  ]
