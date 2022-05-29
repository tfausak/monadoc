module Monadoc.Type.Flag where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Exception.InvalidOption as InvalidOption
import qualified Monadoc.Exception.UnexpectedArgument as UnexpectedArgument
import qualified Monadoc.Exception.UnknownOption as UnknownOption
import qualified System.Console.GetOpt as Console

data Flag
  = Base String
  | Data String
  | Hackage String
  | Help
  | Host String
  | Port String
  | Severity String
  | Sql String
  | Version
  deriving (Eq, Show)

fromArguments :: Exception.MonadThrow m => [String] -> m [Flag]
fromArguments arguments = do
  let (flags, args, opts, errs) =
        Console.getOpt' Console.Permute options arguments
  mapM_ (Exception.throwM . InvalidOption.InvalidOption) errs
  mapM_ (Exception.throwM . UnknownOption.UnknownOption) opts
  mapM_ (Exception.throwM . UnexpectedArgument.UnexpectedArgument) args
  pure flags

options :: [Console.OptDescr Flag]
options =
  [ Console.Option
      ['h', '?']
      ["help"]
      (Console.NoArg Help)
      "Shows this help message, then exits.",
    Console.Option
      ['v']
      ["version"]
      (Console.NoArg Version)
      "Shows the version number, then exits.",
    Console.Option
      []
      ["base-url"]
      (Console.ReqArg Base "URL")
      "Sets the base URL prefix.\nDefault: /",
    Console.Option
      []
      ["data-directory"]
      (Console.ReqArg Data "DIRECTORY")
      "Sets the directory to read data files from.\nDefault: monadoc_datadir environment variable",
    Console.Option
      []
      ["database-file"]
      (Console.ReqArg Sql "FILE")
      "Sets the database file to use. Can be ':memory:'.\nDefault: monadoc.sqlite",
    Console.Option
      []
      ["hackage-url"]
      (Console.ReqArg Hackage "URL")
      "Sets the base Hackage URL to use.\nDefault: https://hackage.haskell.org/",
    Console.Option
      []
      ["host-preference"]
      (Console.ReqArg Host "INTERFACE")
      "Sets the interface to bind to.\nDefault: 127.0.0.1",
    Console.Option
      []
      ["log-severity"]
      (Console.ReqArg Severity "SEVERITY")
      "Sets the minimum log severity. Must be one of: 'debug', 'info', 'warn', or 'error'.\nDefault: debug",
    Console.Option
      []
      ["port-number"]
      (Console.ReqArg Port "NUMBER")
      "Sets the port number to listen on.\nDefault: 3000"
  ]
