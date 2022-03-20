{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.HackageUser.Upsert where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Vendor.Witch as Witch

run :: HackageUser.HackageUser -> App.App HackageUser.Model
run hackageUser = App.withConnection $ \connection -> App.lift $ do
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
      [Sql.Only key] <-
        Sql.query
          connection
          (Witch.into @Sql.Query "select key from hackageUser where id = ?")
          [HackageUser.id hackageUser]
      pure Model.Model {Model.key = key, Model.value = hackageUser}
    model : _ -> pure model
