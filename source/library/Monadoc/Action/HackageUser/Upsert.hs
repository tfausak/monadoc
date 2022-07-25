module Monadoc.Action.HackageUser.Upsert where

import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: HackageUser.HackageUser -> App.App HackageUser.Model
run hackageUser = do
  models <- App.Sql.query "select * from hackageUser where name = ?" [HackageUser.name hackageUser]
  case models of
    [] -> do
      App.Sql.execute "insert into hackageUser (name) values (?)" hackageUser
      key <- Key.SelectLastInsert.run
      pure Model.Model {Model.key = key, Model.value = hackageUser}
    model : _ -> pure model
