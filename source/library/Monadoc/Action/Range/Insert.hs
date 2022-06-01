{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Range.Insert where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.Range as Range
import qualified Monadoc.Type.Model as Model

run :: (MonadSql.MonadSql m, Exception.MonadThrow m) => Range.Range -> m Range.Model
run range = do
  MonadSql.execute "insert into range (\"constraint\") values (?)" range
  key <- Key.SelectLastInsert.run
  pure Model.Model {Model.key = key, Model.value = range}
