module Monadoc.Model.Component where

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.ComponentName as ComponentName
import qualified Monadoc.Type.ComponentType as ComponentType
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Test.QuickCheck as QuickCheck

type Key = Key.Key Component

type Model = Model.Model Component

-- TODO: Handle private components?
-- https://cabal.readthedocs.io/en/stable/cabal-package.html#pkg-field-library-visibility
data Component = Component
  { type_ :: ComponentType.ComponentType,
    name :: ComponentName.ComponentName
  }
  deriving (Eq, Show)

instance Sql.FromRow Component where
  fromRow =
    Component
      <$> Sql.field
      <*> Sql.field

instance Sql.ToRow Component where
  toRow packageMeta =
    [ Sql.toField packageMeta.type_,
      Sql.toField packageMeta.name
    ]

instance QuickCheck.Arbitrary Component where
  arbitrary =
    Component
      <$> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary

migrations :: [Migration.Migration]
migrations =
  [ Migration.new
      (2022, 6, 18, 9, 34, 0)
      "create table component \
      \ ( key integer primary key \
      \ , type text not null \
      \ , name text not null \
      \ , unique ( type, name ) )"
  ]
