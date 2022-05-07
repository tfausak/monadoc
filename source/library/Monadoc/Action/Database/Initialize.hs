{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Database.Initialize where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Constant.Migration as Migration
import qualified Monadoc.Constant.Pragma as Pragma
import qualified Monadoc.Exception.MigrationMismatch as MigrationMismatch
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.Model as Model

run :: (MonadLog.MonadLog m, MonadSql.MonadSql m, Exception.MonadThrow m) => m ()
run = do
  MonadLog.info "initializing database"
  runPragmas
  runMigrations
  MonadLog.debug "initialized database"

runPragmas :: (MonadLog.MonadLog m, MonadSql.MonadSql m) => m ()
runPragmas = do
  MonadLog.info "executing pragmas"
  mapM_ runPragma Pragma.all

runPragma :: (MonadLog.MonadLog m, MonadSql.MonadSql m) => Sql.Query -> m ()
runPragma pragma = do
  MonadLog.debug $ "executing pragma: " <> Text.pack (show pragma)
  MonadSql.execute_ pragma

runMigrations :: (MonadLog.MonadLog m, MonadSql.MonadSql m, Exception.MonadThrow m) => m ()
runMigrations = do
  MonadLog.info "running migrations"
  MonadSql.execute_ Migration.createTable
  mapM_ runMigration Migration.all

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
