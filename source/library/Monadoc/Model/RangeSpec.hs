{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Model.RangeSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Model.Range as Range
import qualified Monadoc.Test.Common as Test
import qualified Monadoc.Type.Constraint as Constraint
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Model.Range" $ do
  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlRow
      Range.Range
        { Range.constraint = Constraint.any
        }
      [ Sql.SQLText ">=0"
      ]
