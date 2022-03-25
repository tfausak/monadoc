{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.PreferredVersions.Upsert where

import qualified Control.Monad.Trans as Trans
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Extra.SqliteSimple as Sql
import qualified Monadoc.Model.PreferredVersions as PreferredVersions
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Vendor.Witch as Witch

run :: PreferredVersions.PreferredVersions -> App.App PreferredVersions.Model
run preferredVersions = App.withConnection $ \connection -> Trans.lift $ do
  rows <-
    Sql.query
      connection
      (Witch.into @Sql.Query "select key from preferredVersions where package = ?")
      [PreferredVersions.package preferredVersions]
  key <- case rows of
    [] -> do
      Sql.execute
        connection
        (Witch.into @Sql.Query "insert into preferredVersions (package, range) values (?, ?)")
        preferredVersions
      key <- Sql.selectLastInsertRowid connection
      pure $ Witch.into @PreferredVersions.Key key
    Sql.Only key : _ -> do
      Sql.execute
        connection
        (Witch.into @Sql.Query "update preferredVersions set range = ? where package = ?")
        (PreferredVersions.range preferredVersions, PreferredVersions.package preferredVersions)
      pure key
  pure Model.Model {Model.key = key, Model.value = preferredVersions}
