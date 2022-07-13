{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Database.Vacuum where

import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Type.App as App

run :: App.App ()
run = App.Sql.execute_ "pragma incremental_vacuum"
