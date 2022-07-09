module Monadoc.Action.Range.Upsert where

import qualified Monadoc.Action.Range.Insert as Range.Insert
import qualified Monadoc.Model.Range as Range
import qualified Monadoc.Query.Range as Range
import qualified Monadoc.Type.App as App

run :: Range.Range -> App.App Range.Model
run range = do
  maybeModel <- Range.selectByConstraint $ Range.constraint range
  case maybeModel of
    Just model -> pure model
    Nothing -> Range.Insert.run range
