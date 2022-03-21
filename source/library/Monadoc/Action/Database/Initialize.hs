module Monadoc.Action.Database.Initialize where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.List as List
import qualified Database.SQLite.Simple as Sql
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
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Vendor.Witch as Witch

pragmas :: [String]
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

run :: App.App ()
run = do
  App.sayString "running pragmas"
  runPragmas
  App.sayString "running migrations"
  runMigrations
  App.sayString "done initializing database"

runPragmas :: App.App ()
runPragmas = mapM_ runPragma pragmas

runPragma :: String -> App.App ()
runPragma pragma = do
  App.withConnection $ \connection ->
    App.lift . Sql.execute_ connection $ Witch.from pragma

runMigrations :: App.App ()
runMigrations = do
  App.withConnection $ \connection ->
    App.lift $ Sql.execute_ connection Migration.createTable
  mapM_ runMigration migrations

runMigration :: Migration.Migration -> App.App ()
runMigration migration = do
  let createdAt = Migration.createdAt migration
      query = Migration.query migration
  App.withConnection $ \connection -> App.lift $ do
    models <-
      Sql.query
        connection
        (Witch.from "select * from migration where createdAt = ?")
        [createdAt]
    case models of
      [] -> Sql.withTransaction connection $ do
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
