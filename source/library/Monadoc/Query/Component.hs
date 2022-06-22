{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Query.Component where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Type.ComponentName as ComponentName
import qualified Monadoc.Type.ComponentType as ComponentType
import qualified Witch

selectByKeys :: MonadSql.MonadSql m => [Component.Key] -> m [Component.Model]
selectByKeys keys =
  if null keys
    then pure []
    else
      MonadSql.query
        (Witch.from @String $ "select * from component where key in (" <> List.intersperse ',' (replicate (length keys) '?') <> ")")
        keys

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
