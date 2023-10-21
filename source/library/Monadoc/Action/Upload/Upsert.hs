module Monadoc.Action.Upload.Upsert where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.MissingKey as MissingKey
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: Upload.Upload -> App.App Upload.Model
run upload = do
  r1 <-
    App.Sql.query
      "select key from upload where package = ? and version = ? and revision = ? limit 1"
      (upload.package, upload.version, upload.revision)
  key <- case r1 of
    Sql.Only key : _ -> pure key
    [] -> do
      r2 <- App.Sql.query "insert into upload (blob, package, revision, uploadedAt, uploadedBy, version, isPreferred, isLatest) values (?, ?, ?, ?, ?, ?, ?, ?) returning key" upload
      case r2 of
        Sql.Only key : _ -> pure key
        [] -> Traced.throw MissingKey.MissingKey
  pure Model.Model {Model.key = key, Model.value = upload}
