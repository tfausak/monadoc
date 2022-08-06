module Monadoc.Action.HackageUser.Upsert where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.MissingKey as MissingKey
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: HackageUser.HackageUser -> App.App HackageUser.Model
run hackageUser = do
  r1 <-
    App.Sql.query
      "select key from hackageUser where name = ? limit 1"
      [HackageUser.name hackageUser]
  key <- case r1 of
    Sql.Only key : _ -> pure key
    [] -> do
      r2 <- App.Sql.query "insert into hackageUser (name) values (?) returning key" hackageUser
      case r2 of
        Sql.Only key : _ -> pure key
        [] -> Traced.throw MissingKey.MissingKey
  pure Model.Model {Model.key = key, Model.value = hackageUser}
