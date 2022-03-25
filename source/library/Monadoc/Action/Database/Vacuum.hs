{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.Database.Vacuum where

import qualified Control.Monad.Trans as Trans
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Type.App as App
import qualified Monadoc.Vendor.Witch as Witch

run :: App.App ()
run = App.withConnection $ \connection ->
  Trans.lift . Sql.execute_ connection $ Witch.into @Sql.Query "pragma incremental_vacuum"
