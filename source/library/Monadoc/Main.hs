module Monadoc.Main (defaultMain) where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.String as String
import qualified Data.Version as Version
import qualified Monadoc.Exception.InvalidOption as InvalidOption
import qualified Monadoc.Exception.InvalidPort as InvalidPort
import qualified Monadoc.Exception.UnexpectedArgument as UnexpectedArgument
import qualified Monadoc.Exception.UnknownOption as UnknownOption
import qualified Monadoc.Server as Server
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Flag as Flag
import qualified Network.HTTP.Client.TLS as Tls
import qualified Paths_monadoc as Monadoc
import qualified System.Console.GetOpt as Console
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified Text.Read as Read

defaultMain :: IO ()
defaultMain = do
  name <- Environment.getProgName
  arguments <- Environment.getArgs
  mainWith name arguments

mainWith :: String -> [String] -> IO ()
mainWith name arguments = do
  flags <- getFlags arguments
  config <- getConfig flags
  context <- getContext name config
  Server.server context

getFlags :: Exception.MonadThrow m => [String] -> m [Flag.Flag]
getFlags arguments = do
  let (flags, args, opts, errs) =
        Console.getOpt' Console.Permute optDescrs arguments
  mapM_ (Exception.throwM . InvalidOption.InvalidOption) errs
  mapM_ (Exception.throwM . UnknownOption.UnknownOption) opts
  mapM_ (Exception.throwM . UnexpectedArgument.UnexpectedArgument) args
  pure flags

getConfig :: Exception.MonadThrow m => [Flag.Flag] -> m Config.Config
getConfig flags = Monad.foldM applyFlag Config.initial flags

getContext :: String -> Config.Config -> IO Context.Context
getContext name config = do
  Monad.when (Config.help config) $ do
    putStr $ Console.usageInfo name optDescrs
    Exception.throwM Exit.ExitSuccess
  Monad.when (Config.version config) $ do
    putStrLn $ Version.showVersion Monadoc.version
    Exception.throwM Exit.ExitSuccess
  manager <- Tls.newTlsManager
  pure Context.Context {Context.config = config, Context.manager = manager}

optDescrs :: [Console.OptDescr Flag.Flag]
optDescrs =
  [ Console.Option
      ['h', '?']
      ["help"]
      (Console.NoArg Flag.Help)
      "shows this help message",
    Console.Option
      ['v']
      ["version"]
      (Console.NoArg Flag.Version)
      "shows the version number",
    Console.Option
      []
      ["host"]
      (Console.ReqArg Flag.Host "HOST")
      "sets the host name (default: 127.0.0.1)",
    Console.Option
      []
      ["port"]
      (Console.ReqArg Flag.Port "PORT")
      "sets the port number (default: 3000)"
  ]

applyFlag ::
  Exception.MonadThrow m => Config.Config -> Flag.Flag -> m Config.Config
applyFlag config flag = case flag of
  Flag.Help -> pure config {Config.help = True}
  Flag.Host string -> pure config {Config.host = String.fromString string}
  Flag.Port string -> do
    port <-
      either (Exception.throwM . InvalidPort.InvalidPort) pure $
        Read.readEither string
    pure config {Config.port = port}
  Flag.Version -> pure config {Config.version = True}
