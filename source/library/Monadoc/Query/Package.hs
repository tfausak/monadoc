module Monadoc.Query.Package where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.PackageName as PackageName

getByName :: PackageName.PackageName -> App.App (Maybe Package.Model)
getByName =
  fmap Maybe.listToMaybe
    . App.Sql.query "select * from package where name = ? limit 1"
    . List.singleton

getKeys :: App.App [Package.Key]
getKeys = fmap Sql.fromOnly <$> App.Sql.query_ "select key from package"
