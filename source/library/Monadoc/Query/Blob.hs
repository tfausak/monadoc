{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.Blob where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Type.Hash as Hash

selectByHash :: MonadSql.MonadSql m => Hash.Hash -> m (Maybe Blob.Model)
selectByHash hash = do
  blobs <- MonadSql.query "select * from blob where hash = ? limit 1" [hash]
  pure $ Maybe.listToMaybe blobs

selectByKey :: MonadSql.MonadSql m => Blob.Key -> m (Maybe Blob.Model)
selectByKey key = do
  blobs <- MonadSql.query "select * from blob where key = ? limit 1" [key]
  pure $ Maybe.listToMaybe blobs
