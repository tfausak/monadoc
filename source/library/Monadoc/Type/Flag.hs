module Monadoc.Type.Flag where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Exception.InvalidOption as InvalidOption
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Exception.UnexpectedArgument as UnexpectedArgument
import qualified Monadoc.Exception.UnknownOption as UnknownOption
import qualified System.Console.GetOpt as Console

data Flag
  = Base String
  | Data String
  | Dsn String
  | Hackage String
  | Help
  | Host String
  | Port String
  | Salt String
  | Severity String
  | Sha String
  | Sql String
  | Version
  deriving (Eq, Show)

fromArguments :: (Exception.MonadThrow m) => [String] -> m [Flag]
fromArguments arguments = do
  let (flags, args, opts, errs) =
        Console.getOpt' Console.Permute options arguments
  mapM_ (Traced.throw . InvalidOption.InvalidOption) errs
  mapM_ (Traced.throw . UnknownOption.UnknownOption) opts
  mapM_ (Traced.throw . UnexpectedArgument.UnexpectedArgument) args
  pure flags

options :: [Console.OptDescr Flag]
options =
  [ Console.Option
      ['h', '?']
      ["help"]
      (Console.NoArg Help)
      "Shows this help message, then exits.",
    Console.Option
      []
      ["version"]
      (Console.NoArg Version)
      "Shows the version number, then exits.",
    Console.Option
      []
      ["base-url"]
      (Console.ReqArg Base "URL")
      "Sets the base URL prefix. (Default: '/')",
    Console.Option
      []
      ["commit-sha"]
      (Console.ReqArg Sha "SHA")
      "Sets the commit SHA. (Default: '')",
    Console.Option
      []
      ["data-directory"]
      (Console.ReqArg Data "DIRECTORY")
      "Sets the directory to read data files from. (Default: 'data')",
    Console.Option
      []
      ["database-file"]
      (Console.ReqArg Sql "FILE")
      "Sets the database file to use or ':memory:' for an in-memory database. (Default: 'monadoc.sqlite')",
    Console.Option
      []
      ["hackage-url"]
      (Console.ReqArg Hackage "URL")
      "Sets the base Hackage URL to use. (Default: 'https://hackage.haskell.org/')",
    Console.Option
      []
      ["host-preference"]
      (Console.ReqArg Host "INTERFACE")
      "Sets the interface to bind to. (Default: '127.0.0.1')",
    Console.Option
      []
      ["log-severity"]
      (Console.ReqArg Severity "SEVERITY")
      "Sets the minimum log severity. Must be one of: 'debug', 'info', 'warn', or 'error'. (Default: 'debug')",
    Console.Option
      []
      ["port-number"]
      (Console.ReqArg Port "NUMBER")
      "Sets the port number to listen on. (Default: '3000')",
    Console.Option
      []
      ["proxy-salt"]
      (Console.ReqArg Salt "STRING")
      "Sets the salt to use for the proxy's HMAC. (Default: '')",
    Console.Option
      []
      ["sentry-dsn"]
      (Console.ReqArg Dsn "DSN")
      "Sets the Sentry client key. (Default: '')"
  ]
