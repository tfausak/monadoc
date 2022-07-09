module Monadoc.Action.Blob.Upsert where

import qualified Monadoc.Action.Blob.Insert as Blob.Insert
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Query.Blob as Blob
import qualified Monadoc.Type.App as App

run :: Blob.Blob -> App.App Blob.Model
run blob = do
  maybeBlob <- Blob.selectByHash $ Blob.hash blob
  case maybeBlob of
    Nothing -> Blob.Insert.run blob
    Just model -> pure model
