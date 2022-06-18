{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Component.Insert where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Type.Model as Model

run :: (MonadSql.MonadSql m, Exception.MonadThrow m) => Component.Component -> m Component.Model
run component = do
  MonadSql.execute "insert into component (type, name) values (?, ?)" component
  key <- Key.SelectLastInsert.run
  pure Model.Model {Model.key = key, Model.value = component}
