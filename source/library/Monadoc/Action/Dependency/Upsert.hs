module Monadoc.Action.Dependency.Upsert where

import qualified Control.Monad as Monad
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Model.Dependency as Dependency
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: Dependency.Dependency -> App.App Dependency.Model
run dependency = do
  dependencies <-
    App.Sql.query
      "select * \
      \ from dependency \
      \ where packageMetaComponent = ? \
      \ and package = ? \
      \ and component = ? \
      \ limit 1"
      ( Dependency.packageMetaComponent dependency,
        Dependency.package dependency,
        Dependency.component dependency
      )
  case dependencies of
    [] -> do
      App.Sql.execute
        "insert into dependency \
        \ (packageMetaComponent, package, component, range) \
        \ values (?, ?, ?, ?)"
        dependency
      key <- Key.SelectLastInsert.run
      pure Model.Model {Model.key = key, Model.value = dependency}
    model : _ -> do
      let expected = Dependency.range dependency
          actual = Dependency.range $ Model.value model
      Monad.when (actual /= expected) $
        App.Sql.execute
          "update dependency set range = ? where key = ?"
          (Dependency.range dependency, Model.key model)
      pure model {Model.value = dependency}
