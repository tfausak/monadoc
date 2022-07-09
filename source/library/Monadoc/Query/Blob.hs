{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.Blob where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Hash as Hash

selectByHash :: Hash.Hash -> App.App (Maybe Blob.Model)
selectByHash hash = do
  blobs <- App.query "select * from blob where hash = ? limit 1" [hash]
  pure $ Maybe.listToMaybe blobs

selectByKey :: Blob.Key -> App.App (Maybe Blob.Model)
selectByKey key = do
  blobs <- App.query "select * from blob where key = ? limit 1" [key]
  pure $ Maybe.listToMaybe blobs
