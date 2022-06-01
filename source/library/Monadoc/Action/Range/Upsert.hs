module Monadoc.Action.Range.Upsert where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Action.Range.Insert as Range.Insert
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.Range as Range
import qualified Monadoc.Query.Range as Range

run :: (MonadSql.MonadSql m, Exception.MonadThrow m) => Range.Range -> m Range.Model
run range = do
  maybeModel <- Range.selectByConstraint $ Range.constraint range
  case maybeModel of
    Just model -> pure model
    Nothing -> Range.Insert.run range
