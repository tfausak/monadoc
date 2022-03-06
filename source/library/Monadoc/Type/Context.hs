module Monadoc.Type.Context where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Pool as Pool
import qualified Data.Version as Version
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Flag as Flag
import qualified Monadoc.Vendor.SqliteSimple as Sql
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Tls
import qualified Paths_monadoc as Monadoc
import qualified Say as Say
import qualified System.Console.GetOpt as Console
import qualified System.Exit as Exit

data Context = Context
  { config :: Config.Config,
    dataDirectory :: FilePath,
    manager :: Client.Manager,
    pool :: Pool.Pool Sql.Connection
  }

fromConfig :: String -> Config.Config -> IO Context
fromConfig name cfg = do
  Monad.when (Config.help cfg) $ do
    Say.sayString
      . List.dropWhileEnd Char.isSpace
      $ Console.usageInfo name Flag.options
    Exception.throwM Exit.ExitSuccess
  Monad.when (Config.version cfg) $ do
    Say.sayString $ Version.showVersion Monadoc.version
    Exception.throwM Exit.ExitSuccess
  con <- Pool.createPool (Sql.open $ Config.database cfg) Sql.close 1 60 8
  dir <- maybe Monadoc.getDataDir pure $ Config.dataDirectory cfg
  mgr <- Tls.newTlsManager
  pure
    Context
      { config = cfg,
        dataDirectory = dir,
        manager = mgr,
        pool = con
      }
