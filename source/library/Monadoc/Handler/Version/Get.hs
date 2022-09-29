module Monadoc.Handler.Version.Get where

import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Extra.Either as Either
import qualified Monadoc.Extra.Maybe as Maybe
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Template.Version.Get as Template
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Reversion as Reversion
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Witch

handler ::
  PackageName.PackageName ->
  Reversion.Reversion ->
  Handler.Handler
handler packageName reversion _ respond = do
  context <- Reader.ask
  package <- do
    packages <- App.Sql.query "select * from package where name = ? limit 1" [packageName]
    Either.throw . Maybe.note NotFound.NotFound $ Maybe.listToMaybe packages
  version <- do
    versions <- App.Sql.query "select * from version where number = ? limit 1" [Reversion.version reversion]
    Either.throw . Maybe.note NotFound.NotFound $ Maybe.listToMaybe versions
  let revision = Reversion.revision reversion
  upload <- do
    uploads <-
      App.Sql.query
        "select * \
        \ from upload \
        \ where package = ? \
        \ and version = ? \
        \ and revision = ? \
        \ limit 1"
        (Model.key package, Model.key version, revision)
    Either.throw . Maybe.note NotFound.NotFound $ Maybe.listToMaybe uploads
  hackageUser <- do
    hackageUsers <- App.Sql.query "select * from hackageUser where key = ?" [Upload.uploadedBy $ Model.value upload]
    Either.throw . Maybe.note NotFound.NotFound $ Maybe.listToMaybe hackageUsers
  maybeLatest <-
    Maybe.listToMaybe
      <$> App.Sql.query
        "select * \
        \ from upload \
        \ inner join version \
        \ on version.key = upload.version \
        \ where upload.package = ? \
        \ and upload.isLatest = true \
        \ and upload.key != ? \
        \ limit 1"
        (Model.key package, Model.key upload)
  packageMeta <- do
    packageMetas <- App.Sql.query "select * from packageMeta where upload = ? limit 1" [Model.key upload]
    Either.throw . Maybe.note NotFound.NotFound $ Maybe.listToMaybe packageMetas
  components <- App.Sql.query "select * from packageMetaComponent inner join component on component.key = packageMetaComponent.component where packageMetaComponent.packageMeta = ?" [Model.key packageMeta]
  let eTag = Common.makeETag . Upload.uploadedAt $ Model.value upload
      breadcrumbs =
        [ Breadcrumb.Breadcrumb {Breadcrumb.label = "Home", Breadcrumb.route = Just Route.Home},
          Breadcrumb.Breadcrumb {Breadcrumb.label = Witch.into @Text.Text packageName, Breadcrumb.route = Just $ Route.Package packageName},
          Breadcrumb.Breadcrumb {Breadcrumb.label = Witch.into @Text.Text reversion, Breadcrumb.route = Nothing}
        ]
  respond
    . Common.htmlResponse Http.ok200 [(Http.hETag, eTag)]
    $ Template.render context breadcrumbs package version upload hackageUser maybeLatest packageMeta components

selectFirst :: Exception.MonadThrow m => m [a] -> m a
selectFirst query = do
  rows <- query
  case rows of
    [] -> Traced.throw NotFound.NotFound
    row : _ -> pure row
