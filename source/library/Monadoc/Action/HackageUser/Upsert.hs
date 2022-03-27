{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.HackageUser.Upsert where

import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans as Trans
import qualified Data.Pool as Pool
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Extra.SqliteSimple as Sql
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Vendor.Witch as Witch

run :: HackageUser.HackageUser -> App.App HackageUser.Model
run hackageUser = do
  context <- Reader.ask
  Pool.withResource (Context.pool context) $ \connection -> Trans.lift $ do
    rows <-
      Sql.query
        connection
        (Witch.into @Sql.Query "select * from hackageUser where id = ? or name = ?")
        (HackageUser.id hackageUser, HackageUser.name hackageUser)
    case rows of
      [] -> do
        Sql.execute
          connection
          (Witch.into @Sql.Query "insert into hackageUser (id, name) values (?, ?)")
          hackageUser
        key <- Sql.selectLastInsertRowid connection
        pure Model.Model {Model.key = Witch.into @HackageUser.Key key, Model.value = hackageUser}
      model : _ -> pure model
