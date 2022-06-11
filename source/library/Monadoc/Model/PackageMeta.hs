{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Model.PackageMeta where

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Model.License as License
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Type.BuildType as BuildType
import qualified Monadoc.Type.Hash as Hash
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Test.QuickCheck as QuickCheck

type Key = Key.Key PackageMeta

type Model = Model.Model PackageMeta

data PackageMeta = PackageMeta
  { buildType :: BuildType.BuildType,
    cabalVersion :: Version.Key,
    hash :: Hash.Hash,
    license :: License.Key,
    upload :: Upload.Key
  }
  deriving (Eq, Show)

instance Sql.FromRow PackageMeta where
  fromRow =
    PackageMeta
      <$> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field

instance Sql.ToRow PackageMeta where
  toRow packageMeta =
    [ Sql.toField $ buildType packageMeta,
      Sql.toField $ cabalVersion packageMeta,
      Sql.toField $ hash packageMeta,
      Sql.toField $ license packageMeta,
      Sql.toField $ upload packageMeta
    ]

instance QuickCheck.Arbitrary PackageMeta where
  arbitrary =
    PackageMeta
      <$> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary

migrations :: [Migration.Migration]
migrations =
  [ Migration.new
      (2022, 6, 10, 17, 40, 0)
      "create table packageMeta \
      \ ( key integer primary key \
      \ , buildType text not null \
      \ , cabalVersion integer not null references version \
      \ , hash blob not null \
      \ , license integer not null references license \
      \ , upload integer not null references upload unique )"
  ]
