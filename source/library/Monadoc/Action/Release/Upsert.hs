module Monadoc.Action.Release.Upsert where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.Release as Release
import qualified Monadoc.Type.Model as Model

run :: (MonadSql.MonadSql m, Exception.MonadThrow m) => Release.Release -> m Release.Model
run release = do
  rows <- MonadSql.query "select * from release where package = ? and version = ? and revision = ?" (Release.package release, Release.version release, Release.revision release)
  case rows of
    model : _ -> pure model
    [] -> do
      MonadSql.execute "insert into release (blob, package, revision, uploadedAt, uploadedBy, version) values (?, ?, ?, ?, ?, ?)" release
      key <- Key.SelectLastInsert.run
      pure Model.Model {Model.key = key, Model.value = release}
