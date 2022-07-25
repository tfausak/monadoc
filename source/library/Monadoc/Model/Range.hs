module Monadoc.Model.Range where

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.Constraint as Constraint
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Test.QuickCheck as QuickCheck

type Model = Model.Model Range

type Key = Key.Key Range

newtype Range = Range
  { constraint :: Constraint.Constraint
  }
  deriving (Eq, Show)

instance Sql.FromRow Range where
  fromRow =
    Range
      <$> Sql.field

instance Sql.ToRow Range where
  toRow version =
    [ Sql.toField $ constraint version
    ]

instance QuickCheck.Arbitrary Range where
  arbitrary =
    Range
      <$> QuickCheck.arbitrary

migrations :: [Migration.Migration]
migrations =
  [ Migration.new
      (2022, 5, 31, 7, 56, 0)
      "create table range \
      \ ( key integer primary key \
      \ , \"constraint\" text not null unique )"
  ]
