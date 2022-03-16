module Monadoc.Type.App where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Pool as Pool
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Client as Client

type App = Reader.ReaderT Context.Context IO

ask :: App Context.Context
ask = Reader.ask

httpLbs :: Client.Request -> App (Client.Response LazyByteString.ByteString)
httpLbs request = do
  context <- ask
  lift . Client.httpLbs request $ Context.manager context

lift :: IO a -> App a
lift = Trans.lift

run :: App a -> Context.Context -> IO a
run = Reader.runReaderT

withConnection :: (Sql.Connection -> App a) -> App a
withConnection f = do
  context <- ask
  Pool.withResource (Context.pool context) f
