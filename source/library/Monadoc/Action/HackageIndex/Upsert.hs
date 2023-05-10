module Monadoc.Action.HackageIndex.Upsert where

import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.HackageIndex.Insert as Insert
import qualified Monadoc.Action.HackageIndex.Update as Update
import qualified Monadoc.Type.App as App

run :: App.App ()
run = do
  rows <- App.Sql.query_ "select * from hackageIndex order by createdAt desc limit 1"
  case rows of
    [] -> Insert.run
    hackageIndex : _ -> Update.run hackageIndex
