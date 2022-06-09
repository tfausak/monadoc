module Monadoc.Action.License.Upsert where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Action.License.Insert as License.Insert
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.License as License
import qualified Monadoc.Query.License as License

run :: (MonadSql.MonadSql m, Exception.MonadThrow m) => License.License -> m License.Model
run license = do
  maybeModel <- License.selectBySpdx $ License.spdx license
  case maybeModel of
    Just model -> pure model
    Nothing -> License.Insert.run license
