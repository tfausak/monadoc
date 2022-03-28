{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Version.Upsert where

import qualified Control.Monad.Catch as Exception
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Type.Model as Model

run :: (MonadSql.MonadSql m, Exception.MonadThrow m) => Version.Version -> m Version.Model
run version = do
  rows <- MonadSql.query "select key from version where number = ?" [Version.number version]
  key <- case rows of
    Sql.Only key : _ -> pure key
    [] -> do
      MonadSql.execute "insert into version (number) values (?)" version
      Key.SelectLastInsert.run
  pure Model.Model {Model.key = key, Model.value = version}
