{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.Release.Upsert where

import qualified Control.Monad.Trans as Trans
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Extra.SqliteSimple as Sql
import qualified Monadoc.Model.Release as Release
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Vendor.Witch as Witch

run :: Release.Release -> App.App Release.Model
run release = App.withConnection $ \connection -> Trans.lift $ do
  rows <-
    Sql.query
      connection
      (Witch.into @Sql.Query "select * from release where package = ? and version = ? and revision = ?")
      (Release.package release, Release.version release, Release.revision release)
  case rows of
    [] -> do
      Sql.execute
        connection
        (Witch.into @Sql.Query "insert into release (blob, package, revision, uploadedAt, uploadedBy, version) values (?, ?, ?, ?, ?, ?)")
        release
      key <- Sql.selectLastInsertRowid connection
      pure Model.Model {Model.key = Witch.into @Release.Key key, Model.value = release}
    model : _ -> pure model
