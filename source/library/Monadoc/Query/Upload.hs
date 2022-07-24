{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.Upload where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Revision as Revision

selectByPackageAndVersionAndRevision ::
  Package.Key ->
  Version.Key ->
  Revision.Revision ->
  App.App (Maybe Upload.Model)
selectByPackageAndVersionAndRevision package version revision = do
  uploads <-
    App.Sql.query
      "select * \
      \ from upload \
      \ where package = ? \
      \ and version = ? \
      \ and revision = ? \
      \ limit 1"
      (package, version, revision)
  pure $ Maybe.listToMaybe uploads
