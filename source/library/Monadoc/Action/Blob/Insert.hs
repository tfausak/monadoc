module Monadoc.Action.Blob.Insert where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.MissingKey as MissingKey
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: Blob.Blob -> App.App Blob.Model
run blob = do
  rows <-
    App.Sql.query
      "insert into blob (size, hash, contents) values (?, ?, ?) returning key"
      blob
  case rows of
    [] -> Traced.throw MissingKey.MissingKey
    Sql.Only key : _ -> pure Model.Model {Model.key = key, Model.value = blob}
