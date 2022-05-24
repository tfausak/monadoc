{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Model.Job where

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Status as Status
import qualified Monadoc.Type.Task as Task
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Test.QuickCheck as QuickCheck

type Model = Model.Model Job

type Key = Key.Key Job

data Job = Job
  { createdAt :: Timestamp.Timestamp,
    finishedAt :: Maybe Timestamp.Timestamp,
    startedAt :: Maybe Timestamp.Timestamp,
    status :: Status.Status,
    task :: Task.Task
  }
  deriving (Eq, Show)

instance Sql.FromRow Job where
  fromRow =
    Job
      <$> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field

instance Sql.ToRow Job where
  toRow job =
    [ Sql.toField $ createdAt job,
      Sql.toField $ finishedAt job,
      Sql.toField $ startedAt job,
      Sql.toField $ status job,
      Sql.toField $ task job
    ]

instance QuickCheck.Arbitrary Job where
  arbitrary =
    Job
      <$> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary

migrations :: [Migration.Migration]
migrations =
  [ Migration.new
      (2022, 1, 6, 0, 0, 0)
      "create table job \
      \ ( key integer primary key \
      \ , createdAt text not null \
      \ , finishedAt text \
      \ , startedAt text \
      \ , status text not null \
      \ , task blob not null )",
    Migration.new
      (2022, 5, 23, 6, 54, 0)
      "create index job_status on job ( status )",
    Migration.new
      (2022, 5, 23, 6, 55, 0)
      "create index job_createdAt on job ( createdAt )"
  ]
