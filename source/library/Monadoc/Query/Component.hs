{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.Component where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Type.ComponentName as ComponentName
import qualified Monadoc.Type.ComponentType as ComponentType

selectByTypeAndName ::
  MonadSql.MonadSql m =>
  ComponentType.ComponentType ->
  ComponentName.ComponentName ->
  m (Maybe Component.Model)
selectByTypeAndName type_ name = do
  rows <-
    MonadSql.query
      "select * from component where type = ? and name = ? limit 1"
      (type_, name)
  pure $ Maybe.listToMaybe rows
