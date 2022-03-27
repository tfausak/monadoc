{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.Package.Upsert where

import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans as Trans
import qualified Data.Pool as Pool
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Extra.SqliteSimple as Sql
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Vendor.Witch as Witch

run :: Package.Package -> App.App Package.Model
run package = do
  context <- Reader.ask
  Pool.withResource (Context.pool context) $ \connection -> Trans.lift $ do
    rows <-
      Sql.query
        connection
        (Witch.into @Sql.Query "select key from package where name = ?")
        [Package.name package]
    key <- case rows of
      [] -> do
        Sql.execute
          connection
          (Witch.into @Sql.Query "insert into package (name) values (?)")
          package
        key <- Sql.selectLastInsertRowid connection
        pure $ Witch.into @Package.Key key
      Sql.Only key : _ -> pure key
    pure Model.Model {Model.key = key, Model.value = package}
