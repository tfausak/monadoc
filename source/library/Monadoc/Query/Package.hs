module Monadoc.Query.Package where

import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.PackageName as PackageName

getByName :: PackageName.PackageName -> App.App Package.Model
getByName name = do
  packages <- App.Sql.query "select * from package where name = ? limit 1" [name]
  NotFound.fromList packages
