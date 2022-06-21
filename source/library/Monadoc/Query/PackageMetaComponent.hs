{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.PackageMetaComponent where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Model.PackageMetaComponent as PackageMetaComponent

selectByPackageMeta ::
  MonadSql.MonadSql m =>
  PackageMeta.Key ->
  m [PackageMetaComponent.Model]
selectByPackageMeta packageMeta =
  MonadSql.query
    "select * from packageMetaComponent where packageMeta = ?"
    [packageMeta]

selectByPackageMetaAndComponent ::
  MonadSql.MonadSql m =>
  PackageMeta.Key ->
  Component.Key ->
  m (Maybe PackageMetaComponent.Model)
selectByPackageMetaAndComponent packageMeta component = do
  rows <-
    MonadSql.query
      "select * from packageMetaComponent where packageMeta = ? and component = ? limit 1"
      (packageMeta, component)
  pure $ Maybe.listToMaybe rows
