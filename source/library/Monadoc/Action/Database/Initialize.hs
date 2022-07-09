{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Database.Initialize where

import qualified Data.Text as Text
import qualified Monadoc.Action.Log as Log
import qualified Monadoc.Action.Migration.Migrate as Migration.Migrate
import qualified Monadoc.Constant.Migration as Migration
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Query as Query

run :: App.App ()
run = do
  Log.info "initializing database"
  runPragmas
  runMigrations
  Log.debug "initialized database"

runPragmas :: App.App ()
runPragmas = do
  Log.info "executing pragmas"
  mapM_ runPragma pragmas

runPragma :: Query.Query -> App.App ()
runPragma pragma = do
  Log.debug $ "executing pragma: " <> Text.pack (show pragma)
  App.execute_ pragma

pragmas :: [Query.Query]
pragmas =
  [ "pragma auto_vacuum = 'incremental'",
    "pragma foreign_keys = true",
    "pragma journal_mode = 'wal'"
  ]

runMigrations :: App.App ()
runMigrations = do
  Log.info "running migrations"
  App.execute_ Migration.createTable
  mapM_ Migration.Migrate.run Migration.all
