module Monadoc.Query.Blob where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Hash as Hash

getByHash :: Hash.Hash -> App.App (Maybe Blob.Model)
getByHash =
  fmap Maybe.listToMaybe
    . App.Sql.query "select * from blob where hash = ? limit 1"
    . List.singleton
