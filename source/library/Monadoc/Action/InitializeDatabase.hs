module Monadoc.Action.InitializeDatabase where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.List as List
import qualified Data.Pool as Pool
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Exception.MigrationMismatch as MigrationMismatch
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Witch

run :: App.App ()
run = do
  runPragmas
  runMigrations

runPragmas :: App.App ()
runPragmas = mapM_ runPragma pragmas

runPragma :: String -> App.App ()
runPragma pragma = do
  context <- App.ask
  Pool.withResource (Context.pool context) $ \connection ->
    App.lift . Sql.execute_ connection $ Witch.from pragma

pragmas :: [String]
pragmas =
  [ "pragma auto_vacuum = 'incremental'",
    "pragma foreign_keys = true",
    "pragma journal_mode = 'wal'"
  ]

runMigrations :: App.App ()
runMigrations = do
  context <- App.ask
  Pool.withResource (Context.pool context) $ \connection ->
    App.lift $ Sql.execute_ connection Migration.createTable
  mapM_ runMigration migrations

runMigration :: Migration.Migration -> App.App ()
runMigration migration = do
  let createdAt = Migration.createdAt migration
      query = Migration.query migration
  context <- App.ask
  Pool.withResource (Context.pool context) $ \connection -> do
    models <-
      App.lift $
        Sql.query
          connection
          (Witch.from "select * from migration where createdAt = ?")
          [createdAt]
    case models of
      [] -> App.lift . Sql.withTransaction connection $ do
        Sql.execute_ connection $ Witch.from query
        Sql.execute
          connection
          (Witch.from "insert into migration (createdAt, query) values (?, ?)")
          migration
      model : _ -> do
        let oldQuery = Migration.query (Model.value model)
        Monad.when (oldQuery /= query)
          . Exception.throwM
          $ MigrationMismatch.MigrationMismatch createdAt oldQuery query

migrations :: [Migration.Migration]
migrations =
  List.sortOn Migration.createdAt $
    mconcat
      [ HackageIndex.migrations,
        Migration.migrations
      ]
