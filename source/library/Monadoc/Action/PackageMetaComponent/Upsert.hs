module Monadoc.Action.PackageMetaComponent.Upsert where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Action.PackageMetaComponent.Insert as PackageMetaComponent.Insert
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.PackageMetaComponent as PackageMetaComponent
import qualified Monadoc.Query.PackageMetaComponent as PackageMetaComponent

run ::
  (MonadSql.MonadSql m, Exception.MonadThrow m) =>
  PackageMetaComponent.PackageMetaComponent ->
  m PackageMetaComponent.Model
run component = do
  maybeModel <-
    PackageMetaComponent.selectByPackageMetaAndComponent
      (PackageMetaComponent.packageMeta component)
      (PackageMetaComponent.component component)
  case maybeModel of
    Just model -> pure model
    Nothing -> PackageMetaComponent.Insert.run component
