{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Upload.Upsert where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Type.Model as Model

run :: (MonadSql.MonadSql m, Exception.MonadThrow m) => Upload.Upload -> m Upload.Model
run upload = do
  rows <- MonadSql.query "select * from upload where package = ? and version = ? and revision = ?" (Upload.package upload, Upload.version upload, Upload.revision upload)
  case rows of
    model : _ -> pure model
    [] -> do
      MonadSql.execute "insert into upload (blob, package, revision, uploadedAt, uploadedBy, version) values (?, ?, ?, ?, ?, ?)" upload
      key <- Key.SelectLastInsert.run
      pure Model.Model {Model.key = key, Model.value = upload}
