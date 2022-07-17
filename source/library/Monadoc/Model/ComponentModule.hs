{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Model.ComponentModule where

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Model.Module as Module
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Test.QuickCheck as QuickCheck

type Model = Model.Model ComponentModule

type Key = Key.Key ComponentModule

data ComponentModule = ComponentModule
  { component :: Component.Key,
    module_ :: Module.Key
  }
  deriving (Eq, Show)

instance Sql.FromRow ComponentModule where
  fromRow =
    ComponentModule
      <$> Sql.field
      <*> Sql.field

instance Sql.ToRow ComponentModule where
  toRow cronEntry =
    [ Sql.toField $ component cronEntry,
      Sql.toField $ module_ cronEntry
    ]

instance QuickCheck.Arbitrary ComponentModule where
  arbitrary =
    ComponentModule
      <$> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary

migrations :: [Migration.Migration]
migrations =
  [ Migration.new
      (2022, 7, 17, 11, 52, 0)
      "create table componentModule \
      \ ( key integer primary key \
      \ , component integer not null references component \
      \ , module integer not null references module \
      \ , unique (component, module) )"
  ]
