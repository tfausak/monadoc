module Monadoc.Action.Database.Vacuum where

import qualified Monadoc.Class.MonadSql as MonadSql

run :: MonadSql.MonadSql m => m ()
run = MonadSql.execute_ "pragma incremental_vacuum"
