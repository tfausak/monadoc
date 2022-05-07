{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.HackageUser.Upsert where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Type.Model as Model

run :: (MonadSql.MonadSql m, Exception.MonadThrow m) => HackageUser.HackageUser -> m HackageUser.Model
run hackageUser = do
  models <- MonadSql.query "select * from hackageUser where name = ?" [HackageUser.name hackageUser]
  case models of
    [] -> do
      MonadSql.execute "insert into hackageUser (name) values (?)" hackageUser
      key <- Key.SelectLastInsert.run
      pure Model.Model {Model.key = key, Model.value = hackageUser}
    model : _ -> pure model
