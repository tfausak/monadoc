{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.License where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Model.License as License
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Spdx as Spdx

selectBySpdx :: Spdx.Spdx -> App.App (Maybe License.Model)
selectBySpdx spdx = do
  rows <- App.Sql.query "select * from license where spdx = ? limit 1" [spdx]
  pure $ Maybe.listToMaybe rows
