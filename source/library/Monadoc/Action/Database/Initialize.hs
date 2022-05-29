{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Database.Initialize where

import qualified Control.Monad.Catch as Exception
import qualified Data.Text as Text
import qualified Monadoc.Action.Migration.Migrate as Migration.Migrate
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Constant.Migration as Migration
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.Query as Query

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

runPragma :: (MonadLog.MonadLog m, MonadSql.MonadSql m) => Query.Query -> m ()
runPragma pragma = do
  MonadLog.debug $ "executing pragma: " <> Text.pack (show pragma)
  MonadSql.execute_ pragma

pragmas :: [Query.Query]
pragmas =
  [ "pragma auto_vacuum = 'incremental'",
    "pragma foreign_keys = true",
    "pragma journal_mode = 'wal'"
  ]

runMigrations :: (MonadLog.MonadLog m, MonadSql.MonadSql m, Exception.MonadThrow m) => m ()
runMigrations = do
  MonadLog.info "running migrations"
  MonadSql.execute_ Migration.createTable
  mapM_ Migration.Migrate.run Migration.all
