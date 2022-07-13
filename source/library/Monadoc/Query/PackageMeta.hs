{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.PackageMeta where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Type.App as App

selectByKey :: PackageMeta.Key -> App.App (Maybe PackageMeta.Model)
selectByKey key = do
  rows <- App.Sql.query "select * from packageMeta where key = ? limit 1" [key]
  pure $ Maybe.listToMaybe rows

selectByUpload :: Upload.Key -> App.App (Maybe PackageMeta.Model)
selectByUpload upload = do
  rows <- App.Sql.query "select * from packageMeta where upload = ? limit 1" [upload]
  pure $ Maybe.listToMaybe rows
