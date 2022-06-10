{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Monadoc.Class.MonadSql where

import qualified Control.Monad.Base as Base
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Type.Query as Query
import qualified Witch

class Monad m => MonadSql m where
  withConnection :: (Sql.Connection -> m a) -> m a

  query :: (Sql.ToRow q, Sql.FromRow r) => Query.Query -> q -> m [r]
  default query :: (Base.MonadBase IO m, Sql.ToRow q, Sql.FromRow r) => Query.Query -> q -> m [r]
  query q r = withConnection $ \c -> Base.liftBase $ Sql.query c (Witch.from q) r

query_ :: (MonadSql m, Sql.FromRow r) => Query.Query -> m [r]
query_ = flip query ()

execute :: (MonadSql m, Sql.ToRow q) => Query.Query -> q -> m ()
execute template parameters = do
  rows <- query template parameters
  let _ = rows :: [[Sql.SQLData]]
  pure ()

execute_ :: MonadSql m => Query.Query -> m ()
execute_ = flip execute ()
