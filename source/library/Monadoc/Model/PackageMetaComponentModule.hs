{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Model.PackageMetaComponentModule where

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Model.Module as Module
import qualified Monadoc.Model.PackageMetaComponent as PackageMetaComponent
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Test.QuickCheck as QuickCheck

type Model = Model.Model PackageMetaComponentModule

type Key = Key.Key PackageMetaComponentModule

data PackageMetaComponentModule = PackageMetaComponentModule
  { packageMetaComponent :: PackageMetaComponent.Key,
    module_ :: Module.Key
  }
  deriving (Eq, Show)

instance Sql.FromRow PackageMetaComponentModule where
  fromRow =
    PackageMetaComponentModule
      <$> Sql.field
      <*> Sql.field

instance Sql.ToRow PackageMetaComponentModule where
  toRow cronEntry =
    [ Sql.toField $ packageMetaComponent cronEntry,
      Sql.toField $ module_ cronEntry
    ]

instance QuickCheck.Arbitrary PackageMetaComponentModule where
  arbitrary =
    PackageMetaComponentModule
      <$> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary

migrations :: [Migration.Migration]
migrations =
  [ Migration.new
      (2022, 7, 23, 16, 7, 0)
      "drop table if exists componentModule",
    Migration.new
      (2022, 7, 23, 16, 8, 0)
      "create table packageMetaComponentModule \
      \ ( key integer primary key \
      \ , packageMetaComponent integer not null references packageMetaComponent \
      \ , module integer not null references module \
      \ , unique (packageMetaComponent, module) )"
  ]
