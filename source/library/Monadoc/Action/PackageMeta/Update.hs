{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.PackageMeta.Update where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Type.Model as Model

run :: MonadSql.MonadSql m => PackageMeta.Model -> m ()
run model = do
  let packageMeta = Model.value model
  MonadSql.execute
    "update packageMeta \
    \ set buildType = ? \
    \ , cabalVersion = ? \
    \ , hash = ? \
    \ , license = ? \
    \ , upload = ? \
    \ , author = ? \
    \ , bugReports = ? \
    \ , category = ? \
    \ , copyright = ? \
    \ , description = ? \
    \ , homepage = ? \
    \ , maintainer = ? \
    \ , pkgUrl = ? \
    \ , stability = ? \
    \ , synopsis = ? \
    \ where key = ?"
    (packageMeta Sql.:. [Model.key model])
