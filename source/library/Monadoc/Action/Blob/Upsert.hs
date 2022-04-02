module Monadoc.Action.Blob.Upsert where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Type.Model as Model

run :: (MonadSql.MonadSql m, Exception.MonadThrow m) => Blob.Blob -> m Blob.Model
run blob = do
  models <- MonadSql.query "select * from blob where hash = ?" [Blob.hash blob]
  case models of
    [] -> do
      MonadSql.execute "insert into blob (contents, hash, size) values (?, ?, ?)" blob
      key <- Key.SelectLastInsert.run
      pure Model.Model {Model.key = key, Model.value = blob}
    model : _ -> pure model
