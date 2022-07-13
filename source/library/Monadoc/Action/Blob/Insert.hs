{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Blob.Insert where

import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: Blob.Blob -> App.App Blob.Model
run blob = do
  App.Sql.execute
    "insert into blob (size, hash, contents) values (?, ?, ?)"
    blob
  key <- Key.SelectLastInsert.run
  pure Model.Model {Model.key = key, Model.value = blob}
