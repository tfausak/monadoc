{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.PackageMeta where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Model.Upload as Upload

selectByKey :: MonadSql.MonadSql m => PackageMeta.Key -> m (Maybe PackageMeta.Model)
selectByKey key = do
  rows <- MonadSql.query "select * from packageMeta where key = ? limit 1" [key]
  pure $ Maybe.listToMaybe rows

selectByUpload :: MonadSql.MonadSql m => Upload.Key -> m (Maybe PackageMeta.Model)
selectByUpload upload = do
  rows <- MonadSql.query "select * from packageMeta where upload = ? limit 1" [upload]
  pure $ Maybe.listToMaybe rows
