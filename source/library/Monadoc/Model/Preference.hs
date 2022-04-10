module Monadoc.Model.Preference where

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Type.Constraint as Constraint
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Test.QuickCheck as QuickCheck

type Model = Model.Model Preference

type Key = Key.Key Preference

data Preference = Preference
  { constraint :: Constraint.Constraint,
    package :: Package.Key
  }
  deriving (Eq, Show)

instance Sql.FromRow Preference where
  fromRow =
    Preference
      <$> Sql.field
      <*> Sql.field

instance Sql.ToRow Preference where
  toRow preference =
    [ Sql.toField $ constraint preference,
      Sql.toField $ package preference
    ]

instance QuickCheck.Arbitrary Preference where
  arbitrary =
    Preference
      <$> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary

migrations :: [Migration.Migration]
migrations =
  [ Migration.new
      (2022, 4, 7, 0, 0, 0)
      "create table preference \
      \ ( key integer primary key \
      \ , \"constraint\" text not null \
      \ , package integer not null unique references package )"
  ]
