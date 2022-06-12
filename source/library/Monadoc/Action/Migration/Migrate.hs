{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Migration.Migrate where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Text as Text
import qualified Monadoc.Action.Migration.Insert as Migration.Insert
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Exception.Mismatch as Mismatch
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Query.Migration as Migration
import qualified Monadoc.Type.Model as Model

run ::
  (MonadLog.MonadLog m, MonadSql.MonadSql m, Exception.MonadThrow m) =>
  Migration.Migration ->
  m Migration.Model
run migration = do
  let createdAt = Migration.createdAt migration
      query = Migration.query migration
  maybeModel <- Migration.selectByCreatedAt createdAt
  case maybeModel of
    Nothing -> do
      MonadLog.debug $ "running migration: " <> Text.pack (show createdAt)
      MonadSql.execute_ query
      Migration.Insert.run migration
    Just model -> do
      let oldQuery = Migration.query $ Model.value model
      Monad.when (oldQuery /= query) $
        Exception.throwM
          Mismatch.Mismatch
            { Mismatch.expected = oldQuery,
              Mismatch.actual = query
            }
      pure model
