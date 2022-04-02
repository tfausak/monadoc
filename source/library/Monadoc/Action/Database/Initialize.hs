module Monadoc.Action.Database.Initialize where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Exception.MigrationMismatch as MigrationMismatch
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Model.Job as Job
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.PreferredVersions as PreferredVersions
import qualified Monadoc.Model.Release as Release
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Type.Model as Model

pragmas :: [Sql.Query]
pragmas =
  [ "pragma auto_vacuum = 'incremental'",
    "pragma foreign_keys = true",
    "pragma journal_mode = 'wal'"
  ]

migrations :: [Migration.Migration]
migrations =
  List.sortOn Migration.createdAt $
    mconcat
      [ Blob.migrations,
        HackageIndex.migrations,
        HackageUser.migrations,
        Job.migrations,
        Migration.migrations,
        Package.migrations,
        PreferredVersions.migrations,
        Release.migrations,
        Version.migrations
      ]

run :: (MonadLog.MonadLog m, MonadSql.MonadSql m, Exception.MonadThrow m) => m ()
run = do
  MonadLog.info "initializing database"
  runPragmas
  runMigrations
  MonadLog.debug "initialized database"

runPragmas :: (MonadLog.MonadLog m, MonadSql.MonadSql m) => m ()
runPragmas = do
  MonadLog.info "executing pragmas"
  mapM_ runPragma pragmas

runPragma :: (MonadLog.MonadLog m, MonadSql.MonadSql m) => Sql.Query -> m ()
runPragma pragma = do
  MonadLog.debug $ "executing pragma: " <> Text.pack (show pragma)
  MonadSql.execute_ pragma

runMigrations :: (MonadLog.MonadLog m, MonadSql.MonadSql m, Exception.MonadThrow m) => m ()
runMigrations = do
  MonadLog.info "running migrations"
  MonadSql.execute_ Migration.createTable
  mapM_ runMigration migrations

runMigration :: (MonadLog.MonadLog m, MonadSql.MonadSql m, Exception.MonadThrow m) => Migration.Migration -> m ()
runMigration migration = do
  let createdAt = Migration.createdAt migration
      query = Migration.query migration
  MonadLog.debug $ "running migration: " <> Text.pack (show createdAt)
  models <- MonadSql.query "select * from migration where createdAt = ?" [createdAt]
  case models of
    [] -> do
      MonadSql.execute_ query
      MonadSql.execute "insert into migration (createdAt, query) values (?, ?)" migration
    model : _ -> do
      let oldQuery = Migration.query $ Model.value model
      Monad.when (oldQuery /= query)
        . Exception.throwM
        $ MigrationMismatch.MigrationMismatch
          { MigrationMismatch.createdAt = createdAt,
            MigrationMismatch.expected = oldQuery,
            MigrationMismatch.actual = query
          }
