{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.ComponentModule where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.ComponentModule as ComponentModule
import qualified Monadoc.Model.Module as Module
import qualified Monadoc.Type.App as App

selectByComponentAndModule ::
  Component.Key ->
  Module.Key ->
  App.App (Maybe ComponentModule.Model)
selectByComponentAndModule component module_ = do
  rows <-
    App.Sql.query
      "select * from componentModule where component = ? and module = ? limit 1"
      (component, module_)
  pure $ Maybe.listToMaybe rows
