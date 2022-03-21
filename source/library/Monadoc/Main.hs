module Monadoc.Main where

import qualified Control.Concurrent.Async as Async
import qualified Control.Monad.Catch as Exception
import qualified Data.Pool as Pool
import qualified GHC.Conc as Conc
import qualified Monadoc.Action.Database.Initialize as Database.Initialize
import qualified Monadoc.Middleware.HandleExceptions as HandleExceptions
import qualified Monadoc.Server.Main as Server
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Flag as Flag
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
  Exception.finally (start context) (stop context)

start :: Context.Context -> IO ()
start context = do
  App.run Database.Initialize.run context
  Async.race_ (Server.server context) (Worker.worker context)

stop :: Context.Context -> IO ()
stop context = do
  Pool.destroyAllResources $ Context.pool context
