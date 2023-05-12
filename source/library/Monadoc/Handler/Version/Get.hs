module Monadoc.Handler.Version.Get where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Handler.Package.Get as Package.Get
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Template.Version.Get as Template
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Reversion as Reversion
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.VersionNumber as VersionNumber
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Witch

handler ::
  PackageName.PackageName ->
  Reversion.Reversion ->
  Handler.Handler
handler packageName reversion _ respond = do
  context <- Reader.ask
  package <- Package.Get.getPackage packageName
  version <- getVersion $ Reversion.version reversion
  upload <- getUpload (Model.key package) (Model.key version) (Reversion.revision reversion)
  hackageUser <- getHackageUser . Upload.uploadedBy $ Model.value upload
  maybeLatest <- getLatestUpload (Model.key package) (Model.key upload)
  packageMeta <- getPackageMeta $ Model.key upload
  components <-
    App.Sql.query
      "select * \
      \ from packageMetaComponent \
      \ inner join component \
      \ on component.key = packageMetaComponent.component \
      \ where packageMetaComponent.packageMeta = ?"
      [Model.key packageMeta]
  let eTag = Common.makeETag . Upload.uploadedAt $ Model.value upload
      breadcrumbs =
        [ Breadcrumb.Breadcrumb {Breadcrumb.label = "Home", Breadcrumb.route = Just Route.Home},
          Breadcrumb.Breadcrumb {Breadcrumb.label = Witch.into @Text.Text packageName, Breadcrumb.route = Just $ Route.Package packageName},
          Breadcrumb.Breadcrumb {Breadcrumb.label = Witch.into @Text.Text reversion, Breadcrumb.route = Nothing}
        ]
  respond
    . Common.htmlResponse
      Http.ok200
      [ (Http.hCacheControl, "max-age=86400, stale-while-revalidate=3600"),
        (Http.hETag, eTag)
      ]
    $ Template.render
      context
      Template.Input
        { Template.breadcrumbs = breadcrumbs,
          Template.package = package,
          Template.version = version,
          Template.upload = upload,
          Template.hackageUser = hackageUser,
          Template.maybeLatest = maybeLatest,
          Template.packageMeta = packageMeta,
          Template.components = components
        }

getVersion :: VersionNumber.VersionNumber -> App.App Version.Model
getVersion number = do
  versions <- App.Sql.query "select * from version where number = ? limit 1" [number]
  NotFound.fromList versions

getUpload :: Package.Key -> Version.Key -> Revision.Revision -> App.App Upload.Model
getUpload package version revision = do
  uploads <-
    App.Sql.query
      "select * \
      \ from upload \
      \ where package = ? \
      \ and version = ? \
      \ and revision = ? \
      \ limit 1"
      (package, version, revision)
  NotFound.fromList uploads

getHackageUser :: HackageUser.Key -> App.App HackageUser.Model
getHackageUser key = do
  hackageUsers <- App.Sql.query "select * from hackageUser where key = ? limit 1" [key]
  NotFound.fromList hackageUsers

getPackageMeta :: Upload.Key -> App.App PackageMeta.Model
getPackageMeta upload = do
  packageMetas <- App.Sql.query "select * from packageMeta where upload = ? limit 1" [upload]
  NotFound.fromList packageMetas

getLatestUpload :: Package.Key -> Upload.Key -> App.App (Maybe (Upload.Model, Version.Model))
getLatestUpload packageKey uploadKey = do
  rows <-
    App.Sql.query
      "select * \
      \ from upload \
      \ inner join version \
      \ on version.key = upload.version \
      \ where upload.package = ? \
      \ and upload.isLatest = true \
      \ and upload.key != ? \
      \ limit 1"
      (packageKey, uploadKey)
  pure $ case rows of
    [] -> Nothing
    (upload Sql.:. version) : _ -> Just (upload, version)
