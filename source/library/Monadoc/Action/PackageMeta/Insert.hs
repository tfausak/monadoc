module Monadoc.Action.PackageMeta.Insert where

import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: PackageMeta.PackageMeta -> App.App PackageMeta.Model
run packageMeta = do
  App.Sql.execute
    "insert into packageMeta \
    \ (buildType, cabalVersion, hash, license, upload, author, bugReports, category, copyright, description, homepage, maintainer, pkgUrl, stability, synopsis) \
    \ values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    packageMeta
  key <- Key.SelectLastInsert.run
  pure Model.Model {Model.key = key, Model.value = packageMeta}
