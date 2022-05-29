{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.Migration where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.Timestamp as Timestamp

selectByCreatedAt :: MonadSql.MonadSql m => Timestamp.Timestamp -> m (Maybe Migration.Model)
selectByCreatedAt createdAt = do
  rows <- MonadSql.query "select * from migration where createdAt = ? limit 1" [createdAt]
  pure $ Maybe.listToMaybe rows
