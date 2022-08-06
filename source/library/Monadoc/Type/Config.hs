module Monadoc.Type.Config where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Monadoc.Exception.InvalidDsn as InvalidDsn
import qualified Monadoc.Extra.Either as Either
import qualified Monadoc.Extra.List as List
import qualified Monadoc.Type.Flag as Flag
import qualified Monadoc.Type.Port as Port
import qualified Monadoc.Type.Severity as Severity
import qualified Network.Wai.Handler.Warp as Warp
import qualified Patrol
import qualified Patrol.Type.Dsn as Patrol.Dsn
import qualified Witch

data Config = Config
  { base :: String,
    data_ :: FilePath,
    dsn :: Maybe Patrol.Dsn,
    hackage :: String,
    help :: Bool,
    host :: Warp.HostPreference,
    port :: Port.Port,
    salt :: Text.Text,
    severity :: Severity.Severity,
    sha :: Text.Text,
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
      data_ = "data",
      dsn = Nothing,
      help = False,
      host = String.fromString "127.0.0.1",
      port = Witch.from @Int 3000,
      salt = Text.empty,
      severity = Severity.Debug,
      sha = Text.empty,
      version = False
    }

applyFlag :: Exception.MonadThrow m => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.Base str -> pure config {base = List.ensureSuffix '/' str}
  Flag.Data str -> pure config {data_ = str}
  Flag.Dsn str ->
    if null str
      then pure config {dsn = Nothing}
      else do
        x <- Either.throw . Bifunctor.first InvalidDsn.InvalidDsn $ Patrol.Dsn.fromString str
        pure config {dsn = Just x}
  Flag.Hackage str -> pure config {hackage = List.ensureSuffix '/' str}
  Flag.Help -> pure config {help = True}
  Flag.Host str -> pure config {host = String.fromString str}
  Flag.Port str -> do
    x <- Either.throw $ Witch.tryInto @Port.Port str
    pure config {port = x}
  Flag.Salt str -> pure config {salt = Witch.from str}
  Flag.Severity svr -> do
    x <- Either.throw $ Witch.tryInto @Severity.Severity svr
    pure config {severity = x}
  Flag.Sha str -> pure config {sha = Witch.from str}
  Flag.Sql str -> pure config {sql = str}
  Flag.Version -> pure config {version = True}

fromFlags :: Exception.MonadThrow m => [Flag.Flag] -> m Config
fromFlags = Monad.foldM applyFlag initial
