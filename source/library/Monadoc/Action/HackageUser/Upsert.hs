{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.HackageUser.Upsert where

import qualified Control.Monad.Catch as Exception
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Type.Model as Model

run :: (MonadSql.MonadSql m, Exception.MonadThrow m) => HackageUser.HackageUser -> m HackageUser.Model
run hackageUser = do
  rows <- MonadSql.query "select key from hackageUser where id = ? or name = ?" (HackageUser.id hackageUser, HackageUser.name hackageUser)
  key <- case rows of
    Sql.Only key : _ -> pure key
    [] -> do
      MonadSql.execute "insert into hackageUser (id, name) values (?, ?)" hackageUser
      Key.SelectLastInsert.run
  pure Model.Model {Model.key = key, Model.value = hackageUser}
