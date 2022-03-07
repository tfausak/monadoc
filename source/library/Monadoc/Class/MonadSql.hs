{-# LANGUAGE TypeApplications #-}

module Monadoc.Class.MonadSql where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Database.SQLite.Simple as Sql

class Monad m => MonadSql m where
  query :: (Sql.ToRow q, Sql.FromRow r) => Sql.Connection -> Sql.Query -> q -> m [r]

instance MonadSql IO where
  query = Sql.query

instance MonadSql m => MonadSql (Reader.ReaderT r m) where
  query c q = Trans.lift . query c q

execute :: (MonadSql m, Sql.ToRow q) => Sql.Connection -> Sql.Query -> q -> m ()
execute c q = Monad.void . query @_ @_ @[Sql.SQLData] c q

execute_ :: MonadSql m => Sql.Connection -> Sql.Query -> m ()
execute_ c q = execute c q ()

query_ :: (MonadSql m, Sql.FromRow r) => Sql.Connection -> Sql.Query -> m [r]
query_ c q = query c q ()
