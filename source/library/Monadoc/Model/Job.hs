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
  shrink job =
    Job
      <$> QuickCheck.shrink (createdAt job)
      <*> QuickCheck.shrink (finishedAt job)
      <*> QuickCheck.shrink (startedAt job)
      <*> QuickCheck.shrink (status job)
      <*> QuickCheck.shrink (task job)

migrations :: [Migration.Migration]
migrations =
  [ Migration.new
      (2022, 3, 20, 0, 0, 0)
      "create table job \
      \ ( key integer primary key \
      \ , createdAt text not null \
      \ , finishedAt text \
      \ , startedAt text \
      \ , status text not null \
      \ , task blob not null )"
  ]
