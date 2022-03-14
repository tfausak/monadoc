module Monadoc.Action.HackageIndex.Upsert where

import qualified Data.Maybe as Maybe
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.HackageIndex.Insert as Insert
import qualified Monadoc.Action.HackageIndex.Update as Update
import qualified Monadoc.Model.HackageIndex as HackageIndex
import Monadoc.Orphanage ()
import qualified Monadoc.Type.App as App
import qualified Witch

run :: App.App HackageIndex.Model
run = do
  hackageIndex <- App.withConnection $ \connection ->
    fmap Maybe.listToMaybe
      . App.lift
      . Sql.query_ connection
      $ Witch.from "select * from hackageIndex"
  maybe Insert.run Update.run hackageIndex
