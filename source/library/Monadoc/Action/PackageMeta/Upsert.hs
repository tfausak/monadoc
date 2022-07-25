module Monadoc.Action.PackageMeta.Upsert where

import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.PackageMeta.Insert as PackageMeta.Insert
import qualified Monadoc.Action.PackageMeta.Update as PackageMeta.Update
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: PackageMeta.PackageMeta -> App.App PackageMeta.Model
run packageMeta = do
  packageMetas <- App.Sql.query "select * from packageMeta where upload = ? limit 1" [PackageMeta.upload packageMeta]
  case packageMetas of
    [] -> PackageMeta.Insert.run packageMeta
    model : _ -> do
      let newModel = model {Model.value = packageMeta}
      PackageMeta.Update.run newModel
      pure newModel
