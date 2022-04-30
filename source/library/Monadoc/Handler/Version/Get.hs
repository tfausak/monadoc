module Monadoc.Handler.Version.Get where

import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Template.Version.Get as Template
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.VersionNumber as VersionNumber
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

handler ::
  (Reader.MonadReader Context.Context m, MonadSql.MonadSql m, Exception.MonadThrow m) =>
  PackageName.PackageName ->
  VersionNumber.VersionNumber ->
  Wai.Request ->
  m Wai.Response
handler packageName versionNumber _ = do
  context <- Reader.ask
  package <- do
    rows <- MonadSql.query "select * from package where name = ?" [packageName]
    case rows of
      [] -> Exception.throwM NotFound.NotFound
      row : _ -> pure row
  version <- do
    rows <- MonadSql.query "select * from version where number = ?" [versionNumber]
    case rows of
      [] -> Exception.throwM NotFound.NotFound
      row : _ -> pure row
  upload <- do
    rows <- MonadSql.query "select * from upload where package = ? and version = ? order by revision desc limit 1" (Model.key package, Model.key version)
    case rows of
      [] -> Exception.throwM NotFound.NotFound
      row : _ -> pure row
  pure . Common.htmlResponse Http.ok200 [] $ Template.render context package version upload
