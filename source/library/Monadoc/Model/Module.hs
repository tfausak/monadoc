module Monadoc.Model.Module where

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.ModuleName as ModuleName
import qualified Test.QuickCheck as QuickCheck

type Model = Model.Model Module

type Key = Key.Key Module

newtype Module = Module
  { name :: ModuleName.ModuleName
  }
  deriving (Eq, Show)

instance Sql.FromRow Module where
  fromRow =
    Module
      <$> Sql.field

instance Sql.ToRow Module where
  toRow package =
    [ Sql.toField package.name
    ]

instance QuickCheck.Arbitrary Module where
  arbitrary =
    Module
      <$> QuickCheck.arbitrary

migrations :: [Migration.Migration]
migrations =
  [ Migration.new
      (2022, 7, 11, 21, 10, 0)
      "create table module \
      \ ( key integer primary key \
      \ , name text not null unique )"
  ]
