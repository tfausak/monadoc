{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Database.Vacuum where

import qualified Monadoc.Type.App as App

run :: App.App ()
run = App.execute_ "pragma incremental_vacuum"
