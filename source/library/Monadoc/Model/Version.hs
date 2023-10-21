module Monadoc.Model.Version where

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.VersionNumber as VersionNumber
import qualified Test.QuickCheck as QuickCheck

type Model = Model.Model Version

type Key = Key.Key Version

newtype Version = Version
  { number :: VersionNumber.VersionNumber
  }
  deriving (Eq, Show)

instance Sql.FromRow Version where
  fromRow =
    Version
      <$> Sql.field

instance Sql.ToRow Version where
  toRow version =
    [ Sql.toField version.number
    ]

instance QuickCheck.Arbitrary Version where
  arbitrary =
    Version
      <$> QuickCheck.arbitrary

migrations :: [Migration.Migration]
migrations =
  [ Migration.new
      (2022, 1, 9, 0, 0, 0)
      "create table version \
      \ ( key integer primary key \
      \ , number text not null unique )"
  ]
