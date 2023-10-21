module Monadoc.Action.HackageIndex.Prune where

import qualified Control.Monad as Monad
import qualified Formatting as F
import qualified Monadoc.Action.App.Log as App.Log
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model

run :: App.App ()
run = do
  rows <- App.Sql.query_ @HackageIndex.Model "select * from hackageIndex where processedAt is not null order by processedAt desc"
  case rows of
    [] -> App.Log.debug "no hackage indexes to prune"
    keep : rest -> do
      App.Log.debug $ F.sformat ("keeping" F.%+ Key.format) keep.key
      Monad.forM_ rest $ \hackageIndex -> do
        App.Log.debug $ F.sformat ("pruning" F.%+ Key.format) hackageIndex.key
        App.Sql.execute "delete from hackageIndex where key = ?" [hackageIndex.key]
        App.Sql.execute "delete from blob where key = ?" [hackageIndex.value.blob]
