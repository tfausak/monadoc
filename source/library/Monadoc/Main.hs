{-# LANGUAGE TypeApplications #-}

module Monadoc.Main where

import qualified Control.Concurrent.Async as Async
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.List as List
import qualified Data.Pool as Pool
import qualified Database.SQLite.Simple as Sql
import qualified GHC.Conc as Conc
import qualified Monadoc.Exception.MigrationMismatch as MigrationMismatch
import qualified Monadoc.Middleware.HandleExceptions as HandleExceptions
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Server.Main as Server
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Flag as Flag
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Worker.Main as Worker
import qualified System.Environment as Environment
import qualified Witch

defaultMain :: IO ()
defaultMain = do
  name <- Environment.getProgName
  arguments <- Environment.getArgs
  mainWith name arguments

mainWith :: String -> [String] -> IO ()
mainWith name arguments = do
  handler <- Conc.getUncaughtExceptionHandler
  Conc.setUncaughtExceptionHandler $
    Exception.handle handler . HandleExceptions.onException
  flags <- Flag.fromArguments arguments
  config <- Config.fromFlags flags
  context <- Context.fromConfig name config
  runMigrations context
  Async.race_ (Server.server context) (Worker.worker context)

runMigrations :: Context.Context -> IO ()
runMigrations context =
  Pool.withResource (Context.pool context) $ \connection -> do
    Sql.execute_ connection $ Witch.from "pragma auto_vacuum = 'incremental'"
    Sql.execute_ connection $ Witch.from "pragma foreign_keys = true"
    Sql.execute_ connection $ Witch.from "pragma journal_mode = 'wal'"
    Sql.execute_ connection Migration.createTable
    mapM_ (runMigration connection) migrations

runMigration :: Sql.Connection -> Migration.Migration -> IO ()
runMigration connection migration = do
  let createdAt = Migration.createdAt migration
      query = Migration.query migration
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

migrations :: [Migration.Migration]
migrations =
  List.sortOn Migration.createdAt $
    mconcat
      [ Migration.migrations
      ]
