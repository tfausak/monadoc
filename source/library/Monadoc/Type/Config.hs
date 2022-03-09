module Monadoc.Type.Config where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.String as String
import qualified Monadoc.Exception.InvalidPort as InvalidPort
import qualified Monadoc.Type.Flag as Flag
import qualified Network.Wai.Handler.Warp as Warp
import qualified Text.Read as Read

data Config = Config
  { base :: String,
    data_ :: Maybe FilePath,
    help :: Bool,
    host :: Warp.HostPreference,
    port :: Warp.Port,
    sql :: FilePath,
    version :: Bool
  }
  deriving (Eq, Show)

initial :: Config
initial =
  Config
    { base = "/",
      sql = "monadoc.sqlite",
      data_ = Nothing,
      help = False,
      host = String.fromString "127.0.0.1",
      port = 3000,
      version = False
    }

applyFlag :: Exception.MonadThrow m => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.Base str -> pure config {base = str}
  Flag.Data str -> pure config {data_ = Just str}
  Flag.Help -> pure config {help = True}
  Flag.Host str -> pure config {host = String.fromString str}
  Flag.Port str -> do
    int <-
      either (Exception.throwM . InvalidPort.InvalidPort) pure $
        Read.readEither str
    pure config {port = int}
  Flag.Sql str -> pure config {sql = str}
  Flag.Version -> pure config {version = True}

fromFlags :: Exception.MonadThrow m => [Flag.Flag] -> m Config
fromFlags = Monad.foldM applyFlag initial
