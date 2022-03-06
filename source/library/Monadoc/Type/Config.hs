module Monadoc.Type.Config where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.String as String
import qualified Monadoc.Exception.InvalidPort as InvalidPort
import qualified Monadoc.Type.Flag as Flag
import qualified Network.Wai.Handler.Warp as Warp
import qualified Text.Read as Read

data Config = Config
  { baseUrl :: String,
    database :: FilePath,
    dataDirectory :: Maybe FilePath,
    help :: Bool,
    host :: Warp.HostPreference,
    port :: Warp.Port,
    version :: Bool
  }
  deriving (Eq, Show)

initial :: Config
initial =
  Config
    { baseUrl = "/",
      database = "monadoc.sqlite",
      dataDirectory = Nothing,
      help = False,
      host = String.fromString "127.0.0.1",
      port = 3000,
      version = False
    }

applyFlag :: Exception.MonadThrow m => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.BaseUrl str -> pure config {baseUrl = str}
  Flag.Database str -> pure config {database = str}
  Flag.DataDirectory str -> pure config {dataDirectory = Just str}
  Flag.Help -> pure config {help = True}
  Flag.Host str -> pure config {host = String.fromString str}
  Flag.Port str -> do
    int <-
      either (Exception.throwM . InvalidPort.InvalidPort) pure $
        Read.readEither str
    pure config {port = int}
  Flag.Version -> pure config {version = True}

fromFlags :: Exception.MonadThrow m => [Flag.Flag] -> m Config
fromFlags = Monad.foldM applyFlag initial
