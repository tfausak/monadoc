{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.Range where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Model.Range as Range
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Constraint as Constraint

selectByConstraint :: Constraint.Constraint -> App.App (Maybe Range.Model)
selectByConstraint constraint = do
  rows <- App.Sql.query "select * from range where \"constraint\" = ? limit 1" [constraint]
  pure $ Maybe.listToMaybe rows
