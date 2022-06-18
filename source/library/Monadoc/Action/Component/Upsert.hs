module Monadoc.Action.Component.Upsert where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Action.Component.Insert as Component.Insert
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Query.Component as Component

run ::
  (MonadSql.MonadSql m, Exception.MonadThrow m) =>
  Component.Component ->
  m Component.Model
run component = do
  maybeModel <- Component.selectByTypeAndName (Component.type_ component) (Component.name component)
  case maybeModel of
    Just model -> pure model
    Nothing -> Component.Insert.run component
