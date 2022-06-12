module Monadoc.Action.PackageMeta.Upsert where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Action.PackageMeta.Insert as PackageMeta.Insert
import qualified Monadoc.Action.PackageMeta.Update as PackageMeta.Update
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Query.PackageMeta as PackageMeta
import qualified Monadoc.Type.Model as Model

run :: (MonadSql.MonadSql m, Exception.MonadThrow m) => PackageMeta.PackageMeta -> m PackageMeta.Model
run packageMeta = do
  maybePackageMeta <- PackageMeta.selectByUpload $ PackageMeta.upload packageMeta
  case maybePackageMeta of
    Nothing -> PackageMeta.Insert.run packageMeta
    Just model -> do
      let newModel = model {Model.value = packageMeta}
      PackageMeta.Update.run newModel
      pure newModel
