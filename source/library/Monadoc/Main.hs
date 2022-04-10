module Monadoc.Main where

import qualified Control.Concurrent.Async as Async
import qualified Control.Monad.Base as Base
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans.Control as Control
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
import qualified System.IO as IO

defaultMain :: IO ()
defaultMain = do
  name <- Environment.getProgName
  arguments <- Environment.getArgs
  mainWith name arguments

mainWith :: String -> [String] -> IO ()
mainWith name arguments = do
  mapM_ (flip IO.hSetBuffering IO.LineBuffering) [IO.stdout, IO.stderr]

  handler <- Conc.getUncaughtExceptionHandler
  Conc.setUncaughtExceptionHandler $
    Exception.handle handler . HandleExceptions.onException

  flags <- Flag.fromArguments arguments
  config <- Config.fromFlags flags
  context <- Context.fromConfig name config
  Reader.runReaderT (App.runAppT $ Exception.finally start stop) context

start :: App.App ()
start = do
  Database.Initialize.run
  Control.control $ \runInBase ->
    Async.race_
      (runInBase Server.server)
      (runInBase Worker.worker)

stop :: (Base.MonadBase IO m, Reader.MonadReader Context.Context m) => m ()
stop = do
  context <- Reader.ask
  Base.liftBase . Pool.destroyAllResources $ Context.pool context
