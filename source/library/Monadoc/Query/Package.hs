{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.Package where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.PackageName as PackageName

selectByName :: PackageName.PackageName -> App.App (Maybe Package.Model)
selectByName packageName = do
  packages <-
    App.Sql.query
      "select * from package where name = ? limit 1"
      [packageName]
  pure $ Maybe.listToMaybe packages
