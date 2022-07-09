{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.Migration where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Timestamp as Timestamp

selectByCreatedAt :: Timestamp.Timestamp -> App.App (Maybe Migration.Model)
selectByCreatedAt createdAt = do
  rows <- App.query "select * from migration where createdAt = ? limit 1" [createdAt]
  pure $ Maybe.listToMaybe rows
