module Monadoc.Type.Context where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Pool as Pool
import qualified Data.Version as Version
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Class.MonadSay as MonadSay
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Flag as Flag
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Tls
import qualified Paths_monadoc as Monadoc
import qualified System.Console.GetOpt as Console
import qualified System.Directory as Directory
import qualified System.Exit as Exit

data Context = Context
  { config :: Config.Config,
    dataDirectory :: FilePath,
    manager :: Client.Manager,
    pool :: Pool.Pool Sql.Connection,
    temporaryDirectory :: FilePath
  }

fromConfig :: String -> Config.Config -> IO Context
fromConfig name cfg = do
  Monad.when (Config.help cfg) $ do
    MonadSay.sayString
      . List.dropWhileEnd Char.isSpace
      $ Console.usageInfo name Flag.options
    Exception.throwM Exit.ExitSuccess
  Monad.when (Config.version cfg) $ do
    MonadSay.sayString $ Version.showVersion Monadoc.version
    Exception.throwM Exit.ExitSuccess
  Context cfg
    <$> maybe Monadoc.getDataDir pure (Config.dataDirectory cfg)
    <*> Tls.newTlsManager
    <*> Pool.createPool (Sql.open $ Config.database cfg) Sql.close 1 60 8
    <*> Directory.getTemporaryDirectory
