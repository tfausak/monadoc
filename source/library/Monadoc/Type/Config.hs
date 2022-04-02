module Monadoc.Type.Config where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.String as String
import qualified Monadoc.Type.Flag as Flag
import qualified Monadoc.Type.Port as Port
import qualified Network.Wai.Handler.Warp as Warp
import qualified Witch

data Config = Config
  { base :: String,
    data_ :: Maybe FilePath,
    hackage :: String,
    help :: Bool,
    host :: Warp.HostPreference,
    port :: Port.Port,
    sql :: FilePath,
    version :: Bool
  }
  deriving (Eq, Show)

initial :: Config
initial =
  Config
    { base = "/",
      hackage = "https://hackage.haskell.org/",
      sql = "monadoc.sqlite",
      data_ = Nothing,
      help = False,
      host = String.fromString "127.0.0.1",
      port = Witch.from @Int 3000,
      version = False
    }

applyFlag :: Exception.MonadThrow m => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.Base str -> pure config {base = ensureTrailing '/' str}
  Flag.Data str -> pure config {data_ = Just str}
  Flag.Hackage str -> pure config {hackage = ensureTrailing '/' str}
  Flag.Help -> pure config {help = True}
  Flag.Host str -> pure config {host = String.fromString str}
  Flag.Port str -> do
    x <- either Exception.throwM pure $ Witch.tryInto @Port.Port str
    pure config {port = x}
  Flag.Sql str -> pure config {sql = str}
  Flag.Version -> pure config {version = True}

fromFlags :: Exception.MonadThrow m => [Flag.Flag] -> m Config
fromFlags = Monad.foldM applyFlag initial

ensureTrailing :: Eq a => a -> [a] -> [a]
ensureTrailing z xs = case xs of
  [] -> []
  [x] -> if x == z then xs else [x, z]
  x : ys -> x : ensureTrailing z ys
