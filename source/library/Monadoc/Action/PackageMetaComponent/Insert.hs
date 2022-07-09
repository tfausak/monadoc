{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.PackageMetaComponent.Insert where

import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Model.PackageMetaComponent as PackageMetaComponent
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run ::
  PackageMetaComponent.PackageMetaComponent ->
  App.App PackageMetaComponent.Model
run packageMetaComponent = do
  App.execute
    "insert into packageMetaComponent (packageMeta, component) values (?, ?)"
    packageMetaComponent
  key <- Key.SelectLastInsert.run
  pure Model.Model {Model.key = key, Model.value = packageMetaComponent}
