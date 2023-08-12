module Monadoc.Type.Context where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Pool as Pool
import qualified Data.Vault.Lazy as Vault
import qualified Data.Version as Version
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Flag as Flag
import qualified Monadoc.Type.RequestId as RequestId
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.Wai.Middleware.Static as Static
import qualified Paths_monadoc as Monadoc
import qualified Say
import qualified System.Console.GetOpt as Console
import qualified System.Directory as Directory
import qualified System.Exit as Exit

data Context = Context
  { cacheContainer :: Static.CacheContainer,
    config :: Config.Config,
    key :: Vault.Key RequestId.RequestId,
    manager :: Client.Manager,
    pool :: Pool.Pool Sql.Connection,
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
  let poolConfig =
        Pool.defaultPoolConfig
          (Sql.open $ Config.sql cfg)
          Sql.close
          60
          (if Config.sql cfg == ":memory:" then 1 else 8)
  let cachingStrategy = Static.CustomCaching $ \fileMeta ->
        [ (Http.hCacheControl, "max-age=604800, stale-while-revalidate=86400"),
          (Http.hETag, Static.fm_etag fileMeta)
        ]
  Context
    <$> Static.initCaching cachingStrategy
    <*> pure cfg
    <*> Vault.newKey
    <*> Tls.newTlsManager
    <*> Pool.newPool poolConfig
    <*> Directory.getTemporaryDirectory
