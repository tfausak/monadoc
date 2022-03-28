module Monadoc.Class.MonadSql where

import qualified Database.SQLite.Simple as Sql

class Monad m => MonadSql m where
  query :: (Sql.ToRow q, Sql.FromRow r) => Sql.Query -> q -> m [r]

query_ :: (MonadSql m, Sql.FromRow r) => Sql.Query -> m [r]
query_ = flip query ()

execute :: (MonadSql m, Sql.ToRow q) => Sql.Query -> q -> m ()
execute template parameters = do
  rows <- query template parameters
  let _ = rows :: [[Sql.SQLData]]
  pure ()

execute_ :: MonadSql m => Sql.Query -> m ()
execute_ = flip execute ()
