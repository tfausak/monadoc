{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Migration.Insert where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.Model as Model

run ::
  (MonadSql.MonadSql m, Exception.MonadThrow m) =>
  Migration.Migration ->
  m Migration.Model
run migration = do
  MonadSql.execute "insert into migration (createdAt, query) values (?, ?)" migration
  key <- Key.SelectLastInsert.run
  pure Model.Model {Model.key = key, Model.value = migration}
