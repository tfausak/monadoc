{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.PackageMetaComponent.Insert where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.PackageMetaComponent as PackageMetaComponent
import qualified Monadoc.Type.Model as Model

run ::
  (MonadSql.MonadSql m, Exception.MonadThrow m) =>
  PackageMetaComponent.PackageMetaComponent ->
  m PackageMetaComponent.Model
run packageMetaComponent = do
  MonadSql.execute
    "insert into packageMetaComponent (packageMeta, component) values (?, ?)"
    packageMetaComponent
  key <- Key.SelectLastInsert.run
  pure Model.Model {Model.key = key, Model.value = packageMetaComponent}
