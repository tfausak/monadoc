module Monadoc.Handler.Package.Get where

import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Preference as Preference
import qualified Monadoc.Template.Package.Get as Template
import qualified Monadoc.Type.Constraint as Constraint
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

handler :: (Reader.MonadReader Context.Context m, MonadSql.MonadSql m, Exception.MonadThrow m) => PackageName.PackageName -> Wai.Request -> m Wai.Response
handler packageName _ = do
  context <- Reader.ask
  package <- do
    rows <- MonadSql.query "select * from package where name = ?" [packageName]
    case rows of
      [] -> Exception.throwM NotFound.NotFound
      row : _ -> pure row
  constraint <- do
    rows <- MonadSql.query "select * from preference where package = ?" [Model.key package]
    case rows of
      [] -> pure Constraint.any
      row : _ -> pure . Preference.constraint $ Model.value row
  rows <-
    MonadSql.query
      "select * \
      \ from upload \
      \ inner join version \
      \ on version.key = upload.version \
      \ inner join hackageUser \
      \ on hackageUser.key = upload.uploadedBy \
      \ where upload.package = ? \
      \ order by upload.uploadedAt desc"
      [Model.key package]
  pure . Common.htmlResponse Http.ok200 [] $ Template.render context package constraint rows
