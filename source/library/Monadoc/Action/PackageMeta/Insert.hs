{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.PackageMeta.Insert where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Type.Model as Model

run :: (MonadSql.MonadSql m, Exception.MonadThrow m) => PackageMeta.PackageMeta -> m PackageMeta.Model
run packageMeta = do
  MonadSql.execute
    "insert into packageMeta \
    \ (buildType, cabalVersion, hash, license, upload, author, bugReports, category, copyright, description, homepage, maintainer, pkgUrl, stability, synopsis) \
    \ values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    packageMeta
  key <- Key.SelectLastInsert.run
  pure Model.Model {Model.key = key, Model.value = packageMeta}
