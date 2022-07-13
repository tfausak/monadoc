{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Query.Component where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.ComponentName as ComponentName
import qualified Monadoc.Type.ComponentType as ComponentType
import qualified Witch

selectByKeys :: [Component.Key] -> App.App [Component.Model]
selectByKeys keys =
  if null keys
    then pure []
    else
      App.Sql.query
        (Witch.from @String $ "select * from component where key in (" <> List.intersperse ',' (replicate (length keys) '?') <> ")")
        keys

selectByTypeAndName ::
  ComponentType.ComponentType ->
  ComponentName.ComponentName ->
  App.App (Maybe Component.Model)
selectByTypeAndName type_ name = do
  rows <-
    App.Sql.query
      "select * from component where type = ? and name = ? limit 1"
      (type_, name)
  pure $ Maybe.listToMaybe rows
