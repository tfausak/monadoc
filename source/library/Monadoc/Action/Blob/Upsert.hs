module Monadoc.Action.Blob.Upsert where

import qualified Monadoc.Action.Blob.Insert as Blob.Insert
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Query.Blob as Blob.Query
import qualified Monadoc.Type.App as App

run :: Blob.Blob -> App.App Blob.Model
run blob = do
  maybeModel <- Blob.Query.getByHash $ Blob.hash blob
  case maybeModel of
    Nothing -> Blob.Insert.run blob
    Just model -> pure model
