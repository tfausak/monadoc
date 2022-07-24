{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Handler.Version.Get where

import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.PackageMetaComponent as PackageMetaComponent
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Query.Component as Component
import qualified Monadoc.Query.HackageUser as HackageUser
import qualified Monadoc.Query.Package as Package
import qualified Monadoc.Query.PackageMeta as PackageMeta
import qualified Monadoc.Query.PackageMetaComponent as PackageMetaComponent
import qualified Monadoc.Query.Upload as Upload
import qualified Monadoc.Query.Version as Version
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
    maybePackage <- Package.selectByName packageName
    maybe (Traced.throw NotFound.NotFound) pure maybePackage
  version <- do
    maybeVersion <- Version.selectByNumber $ Reversion.version reversion
    maybe (Traced.throw NotFound.NotFound) pure maybeVersion
  let revision = Reversion.revision reversion
  upload <- do
    maybeUpload <- Upload.selectByPackageAndVersionAndRevision (Model.key package) (Model.key version) revision
    maybe (Traced.throw NotFound.NotFound) pure maybeUpload
  hackageUser <- do
    maybeHackageUser <- HackageUser.selectByKey . Upload.uploadedBy $ Model.value upload
    maybe (Traced.throw NotFound.NotFound) pure maybeHackageUser
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
    x <- PackageMeta.selectByUpload $ Model.key upload
    maybe (Traced.throw NotFound.NotFound) pure x
  packageMetaComponents <- PackageMetaComponent.selectByPackageMeta $ Model.key packageMeta
  components <- Component.selectByKeys $ fmap (PackageMetaComponent.component . Model.value) packageMetaComponents
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
