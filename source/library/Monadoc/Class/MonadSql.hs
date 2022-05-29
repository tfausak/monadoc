module Monadoc.Class.MonadSql where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Type.Query as Query

class Monad m => MonadSql m where
  query :: (Sql.ToRow q, Sql.FromRow r) => Query.Query -> q -> m [r]

query_ :: (MonadSql m, Sql.FromRow r) => Query.Query -> m [r]
query_ = flip query ()

execute :: (MonadSql m, Sql.ToRow q) => Query.Query -> q -> m ()
execute template parameters = do
  rows <- query template parameters
  let _ = rows :: [[Sql.SQLData]]
  pure ()

execute_ :: MonadSql m => Query.Query -> m ()
execute_ = flip execute ()
