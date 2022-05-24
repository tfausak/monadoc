{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Model.CronEntry where

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.Guid as Guid
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Schedule as Schedule
import qualified Monadoc.Type.Task as Task
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Test.QuickCheck as QuickCheck

type Model = Model.Model CronEntry

type Key = Key.Key CronEntry

data CronEntry = CronEntry
  { guid :: Maybe Guid.Guid,
    runAt :: Timestamp.Timestamp,
    schedule :: Schedule.Schedule,
    task :: Task.Task
  }
  deriving (Eq, Show)

instance Sql.FromRow CronEntry where
  fromRow =
    CronEntry
      <$> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field

instance Sql.ToRow CronEntry where
  toRow cronEntry =
    [ Sql.toField $ guid cronEntry,
      Sql.toField $ runAt cronEntry,
      Sql.toField $ schedule cronEntry,
      Sql.toField $ task cronEntry
    ]

instance QuickCheck.Arbitrary CronEntry where
  arbitrary =
    CronEntry
      <$> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary

migrations :: [Migration.Migration]
migrations =
  [ Migration.new
      (2022, 1, 3, 0, 0, 0)
      "create table cronEntry \
      \ ( key integer primary key \
      \ , guid blob unique \
      \ , runAt text not null \
      \ , schedule text not null \
      \ , task blob not null )",
    Migration.new
      (2022, 5, 23, 6, 48, 0)
      "create index cronEntry_runAt on cronEntry ( runAt )"
  ]
