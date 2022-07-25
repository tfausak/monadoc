module Monadoc.Action.Range.Upsert where

import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.Range.Insert as Range.Insert
import qualified Monadoc.Model.Range as Range
import qualified Monadoc.Type.App as App

run :: Range.Range -> App.App Range.Model
run range = do
  models <- App.Sql.query "select * from range where \"constraint\" = ? limit 1" [Range.constraint range]
  case models of
    model : _ -> pure model
    [] -> Range.Insert.run range
