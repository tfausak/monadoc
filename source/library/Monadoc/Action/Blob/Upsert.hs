module Monadoc.Action.Blob.Upsert where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Action.Blob.Insert as Blob.Insert
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Query.Blob.SelectByHash as Blob.SelectByHash

run ::
  (MonadSql.MonadSql m, Exception.MonadThrow m) =>
  Blob.Blob ->
  m Blob.Model
run blob = do
  maybeBlob <- Blob.SelectByHash.run $ Blob.hash blob
  case maybeBlob of
    Nothing -> Blob.Insert.run blob
    Just model -> pure model
