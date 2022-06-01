{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.Range where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.Range as Range
import qualified Monadoc.Type.Constraint as Constraint

selectAll :: MonadSql.MonadSql m => m [Range.Model]
selectAll = MonadSql.query_ "select * from range"

selectByConstraint :: MonadSql.MonadSql m => Constraint.Constraint -> m (Maybe Range.Model)
selectByConstraint constraint = do
  rows <- MonadSql.query "select * from range where \"constraint\" = ? limit 1" [constraint]
  pure $ Maybe.listToMaybe rows
