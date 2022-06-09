{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.License.Insert where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.License as License
import qualified Monadoc.Type.Model as Model

run :: (MonadSql.MonadSql m, Exception.MonadThrow m) => License.License -> m License.Model
run license = do
  MonadSql.execute "insert into license (spdx) values (?)" license
  key <- Key.SelectLastInsert.run
  pure Model.Model {Model.key = key, Model.value = license}
