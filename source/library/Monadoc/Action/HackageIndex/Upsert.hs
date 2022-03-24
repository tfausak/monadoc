module Monadoc.Action.HackageIndex.Upsert where

import qualified Data.Maybe as Maybe
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.HackageIndex.Insert as Insert
import qualified Monadoc.Action.HackageIndex.Update as Update
import qualified Monadoc.Type.App as App
import qualified Monadoc.Vendor.Witch as Witch

run :: App.App ()
run = do
  App.sayString "upserting hackage index"
  hackageIndex <- App.withConnection $ \connection ->
    fmap Maybe.listToMaybe
      . App.lift
      . Sql.query_ connection
      $ Witch.from "select key, size from hackageIndex order by key asc limit 1"
  maybe Insert.run (uncurry Update.run) hackageIndex
