{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.PackageMetaComponent where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Model.PackageMetaComponent as PackageMetaComponent
import qualified Monadoc.Type.App as App

selectByPackageMeta ::
  PackageMeta.Key ->
  App.App [PackageMetaComponent.Model]
selectByPackageMeta packageMeta =
  App.query
    "select * from packageMetaComponent where packageMeta = ?"
    [packageMeta]

selectByPackageMetaAndComponent ::
  PackageMeta.Key ->
  Component.Key ->
  App.App (Maybe PackageMetaComponent.Model)
selectByPackageMetaAndComponent packageMeta component = do
  rows <-
    App.query
      "select * from packageMetaComponent where packageMeta = ? and component = ? limit 1"
      (packageMeta, component)
  pure $ Maybe.listToMaybe rows
