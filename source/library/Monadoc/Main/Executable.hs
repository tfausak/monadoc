module Monadoc.Main.Executable where

import qualified Control.Concurrent.Async as Async
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Control as Control
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Pool as Pool
import qualified GHC.Conc as Conc
import qualified Monadoc.Action.App.Log as App.Log
import qualified Monadoc.Action.Database.Initialize as Database.Initialize
import qualified Monadoc.Action.Exception.Log as Exception.Log
import qualified Monadoc.Server.Main as Server
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Flag as Flag
import qualified Monadoc.Worker.Main as Worker
import qualified System.Environment as Environment
import qualified System.IO as IO

executable :: IO ()
executable = do
  name <- Environment.getProgName
  arguments <- Environment.getArgs
  environment <- Environment.getEnvironment
  mainWith name arguments environment

mainWith :: String -> [String] -> [(String, String)] -> IO ()
mainWith name arguments environment = do
  mapM_ (flip IO.hSetBuffering IO.LineBuffering) [IO.stdout, IO.stderr]

  flags <-
    (<>)
      <$> Flag.fromEnvironment environment
      <*> Flag.fromArguments arguments
  config <- Config.fromFlags flags
  context <- Context.fromConfig name config

  handler <- Conc.getUncaughtExceptionHandler
  Conc.setUncaughtExceptionHandler $
    Exception.handle handler . App.run context . Exception.Log.run

  App.run context $ Exception.finally start stop

start :: App.App ()
start = do
  App.Log.info "starting up"
  Database.Initialize.run
  Control.control $ \runInBase ->
    Async.race_
      (runInBase Server.server)
      (runInBase Worker.worker)

stop :: App.App ()
stop = do
  App.Log.info "shutting down"
  context <- Reader.ask
  IO.liftIO . Pool.destroyAllResources $ Context.pool context
