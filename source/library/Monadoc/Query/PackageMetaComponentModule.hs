{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.PackageMetaComponentModule where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Model.Module as Module
import qualified Monadoc.Model.PackageMetaComponent as PackageMetaComponent
import qualified Monadoc.Model.PackageMetaComponentModule as PackageMetaComponentModule
import qualified Monadoc.Type.App as App

selectByPackageMetaComponent ::
  PackageMetaComponent.Key ->
  App.App [PackageMetaComponentModule.Model]
selectByPackageMetaComponent packageMetaComponent =
  App.Sql.query
    "select * \
    \ from packageMetaComponentModule \
    \ where packageMetaComponent = ?"
    [packageMetaComponent]

selectByPackageMetaComponentAndModule ::
  PackageMetaComponent.Key ->
  Module.Key ->
  App.App (Maybe PackageMetaComponentModule.Model)
selectByPackageMetaComponentAndModule packageMetaComponent module_ = do
  rows <-
    App.Sql.query
      "select * \
      \ from packageMetaComponentModule \
      \ where packageMetaComponent = ? \
      \ and module = ? \
      \ limit 1"
      (packageMetaComponent, module_)
  pure $ Maybe.listToMaybe rows
