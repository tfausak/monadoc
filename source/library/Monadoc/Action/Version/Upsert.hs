{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.Version.Upsert where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Vendor.Witch as Witch

run :: Version.Version -> App.App Version.Model
run version = App.withConnection $ \connection -> App.lift $ do
  rows <- Sql.query connection (Witch.into @Sql.Query "select key from version where number = ?") [Version.number version]
  key <- case rows of
    [] -> do
      Sql.execute connection (Witch.into @Sql.Query "insert into version (number) values (?)") version
      [Sql.Only key] <- Sql.query connection (Witch.into @Sql.Query "select key from version where number = ?") [Version.number version]
      pure key
    Sql.Only key : _ -> pure key
  pure Model.Model {Model.key = key, Model.value = version}
