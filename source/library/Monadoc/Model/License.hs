module Monadoc.Model.License where

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Spdx as Spdx
import qualified Test.QuickCheck as QuickCheck

type Model = Model.Model License

type Key = Key.Key License

newtype License = License
  { spdx :: Spdx.Spdx
  }
  deriving (Eq, Show)

instance Sql.FromRow License where
  fromRow =
    License
      <$> Sql.field

instance Sql.ToRow License where
  toRow license =
    [ Sql.toField $ spdx license
    ]

instance QuickCheck.Arbitrary License where
  arbitrary =
    License
      <$> QuickCheck.arbitrary

migrations :: [Migration.Migration]
migrations =
  [ Migration.new
      (2022, 6, 8, 7, 44, 0)
      "create table license \
      \ ( key integer primary key \
      \ , spdx text not null unique )"
  ]
