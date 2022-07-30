module Monadoc.Model.Dependency where

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.PackageMetaComponent as PackageMetaComponent
import qualified Monadoc.Model.Range as Range
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Test.QuickCheck as QuickCheck

type Model = Model.Model Dependency

type Key = Key.Key Dependency

data Dependency = Dependency
  { packageMetaComponent :: PackageMetaComponent.Key,
    package :: Package.Key,
    component :: Component.Key,
    range :: Range.Key
  }
  deriving (Eq, Show)

instance Sql.FromRow Dependency where
  fromRow =
    Dependency
      <$> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field

instance Sql.ToRow Dependency where
  toRow packageMeta =
    [ Sql.toField $ packageMetaComponent packageMeta,
      Sql.toField $ package packageMeta,
      Sql.toField $ component packageMeta,
      Sql.toField $ range packageMeta
    ]

instance QuickCheck.Arbitrary Dependency where
  arbitrary =
    Dependency
      <$> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary

migrations :: [Migration.Migration]
migrations =
  [ Migration.new
      (2022, 7, 30, 10, 20, 0)
      "create table dependency \
      \ ( key integer primary key \
      \ , packageMetaComponent integer not null references packageMetaComponent \
      \ , package integer not null references package \
      \ , component integer not null references component \
      \ , range integer not null references range \
      \ , unique ( packageMetaComponent, package, component ) )"
  ]
