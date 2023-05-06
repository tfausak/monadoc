module Monadoc.Action.Database.Initialize where

import qualified Formatting as F
import qualified Monadoc.Action.App.Log as App.Log
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.Migration.Migrate as Migration.Migrate
import qualified Monadoc.Constant.Migration as Migration
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Query as Query
import qualified Witch

run :: App.App ()
run = do
  App.Log.debug "initializing database"
  runPragmas
  runMigrations
  App.Log.debug "initialized database"

runPragmas :: App.App ()
runPragmas = do
  App.Log.debug "executing pragmas"
  mapM_ runPragma pragmas

runPragma :: Query.Query -> App.App ()
runPragma pragma = do
  App.Log.debug $ F.sformat ("executing pragma:" F.%+ F.stext) (Witch.from pragma)
  App.Sql.execute_ pragma

pragmas :: [Query.Query]
pragmas =
  [ "pragma auto_vacuum = 'incremental'",
    "pragma foreign_keys = true",
    "pragma journal_mode = 'wal'",
    "pragma synchronous = 'normal'"
  ]

runMigrations :: App.App ()
runMigrations = do
  App.Log.debug "running migrations"
  App.Sql.execute_ Migration.createTable
  mapM_ Migration.Migrate.run Migration.all
