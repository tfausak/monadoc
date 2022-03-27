{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.Database.Vacuum where

import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans as Trans
import qualified Data.Pool as Pool
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Vendor.Witch as Witch

run :: App.App ()
run = do
  context <- Reader.ask
  Pool.withResource (Context.pool context) $ \connection ->
    Trans.lift . Sql.execute_ connection $ Witch.into @Sql.Query "pragma incremental_vacuum"
