{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.License where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.License as License
import qualified Monadoc.Type.Spdx as Spdx

selectBySpdx :: MonadSql.MonadSql m => Spdx.Spdx -> m (Maybe License.Model)
selectBySpdx spdx = do
  rows <- MonadSql.query "select * from license where spdx = ? limit 1" [spdx]
  pure $ Maybe.listToMaybe rows
