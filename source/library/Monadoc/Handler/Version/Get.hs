module Monadoc.Handler.Version.Get where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Query.Package as Package.Query
import qualified Monadoc.Query.Version as Version.Query
import qualified Monadoc.Template.Version.Get as Template
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Reversion as Reversion
import qualified Monadoc.Type.Revision as Revision
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
  package <- NotFound.fromMaybe =<< Package.Query.getByName packageName
  version <- NotFound.fromMaybe =<< Version.Query.getByNumber reversion.version
  upload <- getUpload package.key version.key reversion.revision
  hackageUser <- getHackageUser upload.value.uploadedBy
  maybeLatest <- getLatestUpload package.key upload.key
  packageMeta <- getPackageMeta upload.key
  components <-
    App.Sql.query
      "select * \
      \ from packageMetaComponent \
      \ inner join component \
      \ on component.key = packageMetaComponent.component \
      \ where packageMetaComponent.packageMeta = ?"
      [packageMeta.key]
  let eTag = Common.makeETag upload.value.uploadedAt
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
