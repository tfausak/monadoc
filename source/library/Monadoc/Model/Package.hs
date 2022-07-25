module Monadoc.Model.Package where

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Test.QuickCheck as QuickCheck

type Model = Model.Model Package

type Key = Key.Key Package

newtype Package = Package
  { name :: PackageName.PackageName
  }
  deriving (Eq, Show)

instance Sql.FromRow Package where
  fromRow =
    Package
      <$> Sql.field

instance Sql.ToRow Package where
  toRow package =
    [ Sql.toField $ name package
    ]

instance QuickCheck.Arbitrary Package where
  arbitrary =
    Package
      <$> QuickCheck.arbitrary

migrations :: [Migration.Migration]
migrations =
  [ Migration.new
      (2022, 1, 7, 0, 0, 0)
      "create table package \
      \ ( key integer primary key \
      \ , name text not null unique )"
  ]
