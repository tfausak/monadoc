{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Key.SelectLastInsert where

import qualified Control.Monad.Catch as Exception
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Exception.MissingRowid as MissingRowid
import qualified Monadoc.Type.Key as Key

run :: (MonadSql.MonadSql m, Exception.MonadThrow m) => m (Key.Key a)
run = do
  rows <- MonadSql.query_ "select last_insert_rowid()"
  case rows of
    Sql.Only key : _ | key /= Key.zero -> pure key
    _ -> Exception.throwM MissingRowid.MissingRowid
