{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Upload.Upsert where

import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: Upload.Upload -> App.App Upload.Model
run upload = do
  rows <- App.Sql.query "select * from upload where package = ? and version = ? and revision = ?" (Upload.package upload, Upload.version upload, Upload.revision upload)
  case rows of
    model : _ -> pure model
    [] -> do
      App.Sql.execute "insert into upload (blob, package, revision, uploadedAt, uploadedBy, version, isPreferred, isLatest) values (?, ?, ?, ?, ?, ?, ?, ?)" upload
      key <- Key.SelectLastInsert.run
      pure Model.Model {Model.key = key, Model.value = upload}
