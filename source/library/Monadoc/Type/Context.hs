module Monadoc.Type.Context where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Pool as Pool
import qualified Data.Version as Version
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Flag as Flag
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Tls
import qualified Paths_monadoc as Monadoc
import qualified Say
import qualified System.Console.GetOpt as Console
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Exit as Exit

data Context = Context
  { config :: Config.Config,
    data_ :: FilePath,
    manager :: Client.Manager,
    pool :: Pool.Pool Sql.Connection,
    sha :: Maybe String,
    temporaryDirectory :: FilePath
  }

fromConfig :: String -> Config.Config -> IO Context
fromConfig name cfg = do
  let version = Version.showVersion Monadoc.version
  Monad.when (Config.help cfg) $ do
    let header = unwords [name, "version", version]
    Say.sayString
      . List.dropWhileEnd Char.isSpace
      $ Console.usageInfo header Flag.options
    Exception.throwM Exit.ExitSuccess
  Monad.when (Config.version cfg) $ do
    Say.sayString version
    Exception.throwM Exit.ExitSuccess
  Context cfg
    <$> maybe Monadoc.getDataDir pure (Config.data_ cfg)
    <*> Tls.newTlsManager
    <*> Pool.newPool
      Pool.PoolConfig
        { Pool.createResource = Sql.open $ Config.sql cfg,
          Pool.freeResource = Sql.close,
          Pool.poolCacheTTL = 60,
          Pool.poolMaxResources = 8
        }
    <*> Environment.lookupEnv "MONADOC_SHA"
    <*> Directory.getTemporaryDirectory
