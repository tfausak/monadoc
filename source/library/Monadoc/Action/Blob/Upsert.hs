module Monadoc.Action.Blob.Upsert where

import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.Blob.Insert as Blob.Insert
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Type.App as App

run :: Blob.Blob -> App.App Blob.Model
run blob = do
  blobs <-
    App.Sql.query
      "select * from blob where hash = ? limit 1"
      [Blob.hash blob]
  case blobs of
    [] -> Blob.Insert.run blob
    model : _ -> pure model
