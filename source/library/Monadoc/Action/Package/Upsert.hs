module Monadoc.Action.Package.Upsert where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Type.Model as Model

run :: (MonadSql.MonadSql m, Exception.MonadThrow m) => Package.Package -> m Package.Model
run package = do
  models <- MonadSql.query "select * from package where name = ?" [Package.name package]
  case models of
    [] -> do
      MonadSql.execute "insert into package (name) values (?)" package
      key <- Key.SelectLastInsert.run
      pure Model.Model {Model.key = key, Model.value = package}
    model : _ -> pure model
