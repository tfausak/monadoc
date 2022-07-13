{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Migration.Migrate where

import qualified Control.Monad as Monad
import qualified Formatting as F
import qualified Monadoc.Action.App.Log as App.Log
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.Migration.Insert as Migration.Insert
import qualified Monadoc.Exception.Mismatch as Mismatch
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Query.Migration as Migration
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model
import qualified Witch

run ::
  Migration.Migration ->
  App.App Migration.Model
run migration = do
  let createdAt = Migration.createdAt migration
      query = Migration.query migration
  maybeModel <- Migration.selectByCreatedAt createdAt
  case maybeModel of
    Nothing -> do
      App.Log.debug $ F.sformat ("running migration: " F.% F.stext) (Witch.from createdAt)
      App.Sql.execute_ query
      Migration.Insert.run migration
    Just model -> do
      let oldQuery = Migration.query $ Model.value model
      Monad.when (oldQuery /= query) $
        Traced.throw
          Mismatch.Mismatch
            { Mismatch.expected = oldQuery,
              Mismatch.actual = query
            }
      pure model
