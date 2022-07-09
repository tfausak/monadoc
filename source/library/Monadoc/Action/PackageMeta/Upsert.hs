module Monadoc.Action.PackageMeta.Upsert where

import qualified Monadoc.Action.PackageMeta.Insert as PackageMeta.Insert
import qualified Monadoc.Action.PackageMeta.Update as PackageMeta.Update
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Query.PackageMeta as PackageMeta
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: PackageMeta.PackageMeta -> App.App PackageMeta.Model
run packageMeta = do
  maybePackageMeta <- PackageMeta.selectByUpload $ PackageMeta.upload packageMeta
  case maybePackageMeta of
    Nothing -> PackageMeta.Insert.run packageMeta
    Just model -> do
      let newModel = model {Model.value = packageMeta}
      PackageMeta.Update.run newModel
      pure newModel
