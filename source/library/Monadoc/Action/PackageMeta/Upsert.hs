module Monadoc.Action.PackageMeta.Upsert where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.MissingKey as MissingKey
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: PackageMeta.PackageMeta -> App.App PackageMeta.Model
run packageMeta = do
  rows <-
    App.Sql.query
      "insert into packageMeta ( buildType, cabalVersion, hash, license, upload, author, bugReports, category, copyright, description, homepage, maintainer, pkgUrl, stability, synopsis ) values ( ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ? ) \
      \ on conflict ( upload ) do update \
      \ set author = excluded.author \
      \ , bugReports = excluded.bugReports \
      \ , buildType = excluded.buildType \
      \ , cabalVersion = excluded.cabalVersion \
      \ , category = excluded.category \
      \ , copyright = excluded.copyright \
      \ , description = excluded.description \
      \ , hash = excluded.hash \
      \ , homepage = excluded.homepage \
      \ , license = excluded.license \
      \ , maintainer = excluded.maintainer \
      \ , pkgUrl = excluded.pkgUrl \
      \ , stability = excluded.stability \
      \ , synopsis = excluded.synopsis \
      \ , upload = excluded.upload \
      \ returning key"
      packageMeta
  case rows of
    [] -> Traced.throw MissingKey.MissingKey
    Sql.Only key : _ -> pure Model.Model {Model.key = key, Model.value = packageMeta}
