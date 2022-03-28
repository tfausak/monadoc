{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.Key.SelectLastInsert where

import qualified Control.Monad.Catch as Exception
import qualified Data.Int as Int
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Exception.MissingRowid as MissingRowid
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Vendor.Witch as Witch

run :: (MonadSql.MonadSql m, Exception.MonadThrow m) => m (Key.Key a)
run = do
  rows <- MonadSql.query_ "select last_insert_rowid()"
  case rows of
    Sql.Only key : _ | key /= Witch.from @Int.Int64 0 -> pure key
    _ -> Exception.throwM MissingRowid.MissingRowid
