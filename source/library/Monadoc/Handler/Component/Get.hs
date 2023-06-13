module Monadoc.Handler.Component.Get where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Trans.Reader as Reader
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Extra.Exception as Exception
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Handler.Version.Get as Version.Get
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Model.PackageMetaComponent as PackageMetaComponent
import qualified Monadoc.Query.Package as Package.Query
import qualified Monadoc.Query.Version as Version.Query
import qualified Monadoc.Template.Component.Get as Template
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.ComponentId as ComponentId
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Reversion as Reversion
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http
import qualified Witch

handler ::
  PackageName.PackageName ->
  Reversion.Reversion ->
  ComponentId.ComponentId ->
  Handler.Handler
handler packageName reversion componentId _ respond = do
  package <- NotFound.fromMaybe =<< Package.Query.getByName packageName
  version <- NotFound.fromMaybe =<< Version.Query.getByNumber (Reversion.version reversion)
  upload <- Version.Get.getUpload (Model.key package) (Model.key version) (Reversion.revision reversion)
  packageMeta <- Version.Get.getPackageMeta $ Model.key upload
  component <- getComponent componentId
  packageMetaComponent <- getPackageMetaComponent (Model.key packageMeta) (Model.key component)
  packageMetaComponentModules <-
    App.Sql.query
      "select * \
      \ from packageMetaComponentModule \
      \ inner join module \
      \ on module.key = packageMetaComponentModule.module \
      \ where packageMetaComponentModule.packageMetaComponent = ?"
      [Model.key packageMetaComponent]
  dependencies <-
    App.Sql.query
      "select * \
      \ from dependency \
      \ inner join package \
      \ on package.key = dependency.package \
      \ inner join component \
      \ on component.key = dependency.component \
      \ inner join range \
      \ on range.key = dependency.range \
      \ where dependency.packageMetaComponent = ?"
      [Model.key packageMetaComponent]
  -- TODO: This currently only gets direct reverse dependencies. It should also
  -- get indirect ones. And it should try to limit the result set coming back
  -- from SQL as much as possible.
  reverseDependencies <-
    App.Sql.query
      "select * \
      \ from dependency \
      \ inner join packageMetaComponent \
      \ on packageMetaComponent.key = dependency.packageMetaComponent \
      \ inner join packageMeta \
      \ on packageMeta.key = packageMetaComponent.packageMeta \
      \ inner join upload \
      \ on upload.key = packageMeta.upload \
      \ inner join package \
      \ on package.key = upload.package \
      \ inner join version \
      \ on version.key = upload.version \
      \ inner join range \
      \ on range.key = dependency.range \
      \ inner join component \
      \ on component.key = packageMetaComponent.component \
      \ where dependency.package = ? \
      \ and dependency.component = ? \
      \ limit 64"
      ( Model.key package,
        Model.key component
      )
  maybeLatest <- Version.Get.getLatestUpload (Model.key package) (Model.key upload)
  hasComponent <- case maybeLatest of
    Nothing -> pure False
    Just (u, _) -> Exception.handleIf (Exception.isType @NotFound.NotFound) (const $ pure False) $ do
      pm <- Version.Get.getPackageMeta $ Model.key u
      Monad.void $ getPackageMetaComponent (Model.key pm) (Model.key component)
      pure True
  context <- Reader.ask
  let breadcrumbs =
        [ Breadcrumb.Breadcrumb {Breadcrumb.label = "Home", Breadcrumb.route = Just Route.Home},
          Breadcrumb.Breadcrumb {Breadcrumb.label = Witch.from packageName, Breadcrumb.route = Just $ Route.Package packageName},
          Breadcrumb.Breadcrumb {Breadcrumb.label = Witch.from reversion, Breadcrumb.route = Just $ Route.Version packageName reversion},
          Breadcrumb.Breadcrumb {Breadcrumb.label = Witch.from componentId, Breadcrumb.route = Nothing}
        ]
  respond
    . Common.htmlResponse
      Http.ok200
      [ (Http.hCacheControl, "max-age=86400, stale-while-revalidate=3600")
      ]
    $ Template.render
      context
      Template.Input
        { Template.breadcrumbs = breadcrumbs,
          Template.package = package,
          Template.version = version,
          Template.upload = upload,
          Template.maybeLatest = maybeLatest,
          Template.hasComponent = hasComponent,
          Template.packageMeta = packageMeta,
          Template.component = component,
          Template.packageMetaComponent = packageMetaComponent,
          Template.packageMetaComponentModules = packageMetaComponentModules,
          Template.dependencies = dependencies,
          Template.reverseDependencies = reverseDependencies
        }

getComponent :: ComponentId.ComponentId -> App.App Component.Model
getComponent componentId = do
  components <-
    App.Sql.query
      "select * from component where type = ? and name = ? limit 1"
      (ComponentId.type_ componentId, ComponentId.name componentId)
  NotFound.fromList components

getPackageMetaComponent :: PackageMeta.Key -> Component.Key -> App.App PackageMetaComponent.Model
getPackageMetaComponent packageMeta component = do
  packageMetaComponents <-
    App.Sql.query
      "select * \
      \ from packageMetaComponent \
      \ where packageMeta = ? \
      \ and component = ? \
      \ limit 1"
      (packageMeta, component)
  NotFound.fromList packageMetaComponents
