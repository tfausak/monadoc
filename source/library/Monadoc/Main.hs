{-# LANGUAGE TypeApplications #-}

module Monadoc.Main where

import qualified Control.Concurrent.Async as Async
import qualified Control.Monad.Catch as Exception
import qualified Data.Pool as Pool
import qualified Database.SQLite.Simple as Sql
import qualified GHC.Conc as Conc
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Middleware.HandleExceptions as HandleExceptions
import qualified Monadoc.Server.Main as Server
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Flag as Flag
import qualified Monadoc.Vendor.Witch as Witch
import qualified Monadoc.Worker.Main as Worker
import qualified System.Environment as Environment

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
  Pool.withResource (Context.pool context) $ \connection ->
    MonadSql.execute_ connection $
      Witch.into @Sql.Query "pragma journal_mode = wal"
