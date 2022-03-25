module Monadoc.Type.App where

import qualified Control.Monad.Reader as Reader
import qualified Data.Pool as Pool
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Type.Context as Context

type App = Reader.ReaderT Context.Context IO

withConnection :: (Sql.Connection -> App a) -> App a
withConnection f = do
  context <- Reader.ask
  Pool.withResource (Context.pool context) f
