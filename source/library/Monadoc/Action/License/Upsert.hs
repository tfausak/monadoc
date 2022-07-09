module Monadoc.Action.License.Upsert where

import qualified Monadoc.Action.License.Insert as License.Insert
import qualified Monadoc.Model.License as License
import qualified Monadoc.Query.License as License
import qualified Monadoc.Type.App as App

run :: License.License -> App.App License.Model
run license = do
  maybeModel <- License.selectBySpdx $ License.spdx license
  case maybeModel of
    Just model -> pure model
    Nothing -> License.Insert.run license
