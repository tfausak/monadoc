{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.Blob.Upsert where

import qualified Control.Monad.Trans as Trans
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Extra.SqliteSimple as Sql
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Vendor.Witch as Witch

run :: Blob.Blob -> App.App Blob.Model
run blob = App.withConnection $ \connection -> Trans.lift $ do
  rows <-
    Sql.query
      connection
      (Witch.into @Sql.Query "select key from blob where hash = ?")
      [Blob.hash blob]
  key <- case rows of
    [] -> do
      Sql.execute
        connection
        (Witch.into @Sql.Query "insert into blob (contents, hash, size) values (?, ?, ?)")
        blob
      key <- Sql.selectLastInsertRowid connection
      pure $ Witch.into @Blob.Key key
    Sql.Only key : _ -> pure key
  pure Model.Model {Model.key = key, Model.value = blob}
