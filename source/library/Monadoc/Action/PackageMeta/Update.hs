{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.PackageMeta.Update where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: PackageMeta.Model -> App.App ()
run model = do
  let packageMeta = Model.value model
  App.Sql.execute
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
