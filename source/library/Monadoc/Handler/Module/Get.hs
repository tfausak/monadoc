module Monadoc.Handler.Module.Get where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Reader as Reader
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Handler.Version.Get as Version.Get
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.Module as Module
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Model.PackageMetaComponent as PackageMetaComponent
import qualified Monadoc.Model.PackageMetaComponentModule as PackageMetaComponentModule
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Template.Module.Get as Template
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.ComponentId as ComponentId
import qualified Monadoc.Type.ComponentName as ComponentName
import qualified Monadoc.Type.ComponentType as ComponentType
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.ModuleName as ModuleName
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Reversion as Reversion
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.VersionNumber as VersionNumber
import qualified Network.HTTP.Types as Http
import qualified Witch

handler ::
  PackageName.PackageName ->
  Reversion.Reversion ->
  ComponentId.ComponentId ->
  ModuleName.ModuleName ->
  Handler.Handler
handler packageName reversion componentId moduleName _ respond = do
  package <- getPackage packageName
  version <- getVersion $ Reversion.version reversion
  upload <- getUpload (Model.key package) (Model.key version) (Reversion.revision reversion)
  packageMeta <- getPackageMeta $ Model.key upload
  component <- getComponent (ComponentId.type_ componentId) (ComponentId.name componentId)
  packageMetaComponent <- getPackageMetaComponent (Model.key packageMeta) (Model.key component)
  module_ <- getModule moduleName
  Monad.void $ getPackageMetaComponentModule (Model.key packageMetaComponent) (Model.key module_)
  maybeLatest <- Version.Get.getLatestUpload (Model.key package) (Model.key upload)
  context <- Reader.ask
  let route = Route.Module packageName reversion componentId moduleName
      breadcrumbs =
        [ Breadcrumb.Breadcrumb {Breadcrumb.label = "Home", Breadcrumb.route = Just Route.Home},
          Breadcrumb.Breadcrumb {Breadcrumb.label = Witch.from packageName, Breadcrumb.route = Just $ Route.Package packageName},
          Breadcrumb.Breadcrumb {Breadcrumb.label = Witch.from reversion, Breadcrumb.route = Just $ Route.Version packageName reversion},
          Breadcrumb.Breadcrumb {Breadcrumb.label = Witch.from componentId, Breadcrumb.route = Just $ Route.Component packageName reversion componentId},
          Breadcrumb.Breadcrumb {Breadcrumb.label = Witch.from moduleName, Breadcrumb.route = Nothing}
        ]
  respond
    . Common.htmlResponse
      Http.ok200
      [ (Http.hCacheControl, "max-age=86400, stale-while-revalidate=3600")
      ]
    $ Template.render context route breadcrumbs package version upload maybeLatest component module_

getComponent :: ComponentType.ComponentType -> ComponentName.ComponentName -> App.App Component.Model
getComponent componentType componentName = do
  components <-
    App.Sql.query
      "select * from component where type = ? and name = ? limit 1"
      (componentType, componentName)
  NotFound.fromList components

getModule :: ModuleName.ModuleName -> App.App Module.Model
getModule moduleName = do
  modules <-
    App.Sql.query
      "select * from module where name = ? limit 1"
      [moduleName]
  NotFound.fromList modules

getPackage :: PackageName.PackageName -> App.App Package.Model
getPackage packageName = do
  packages <-
    App.Sql.query
      "select * from package where name = ? limit 1"
      [packageName]
  NotFound.fromList packages

getPackageMeta :: Upload.Key -> App.App PackageMeta.Model
getPackageMeta upload = do
  packageMetas <-
    App.Sql.query
      "select * from packageMeta where upload = ? limit 1"
      [upload]
  NotFound.fromList packageMetas

getPackageMetaComponent :: PackageMeta.Key -> Component.Key -> App.App PackageMetaComponent.Model
getPackageMetaComponent packageMeta component = do
  packageMetaComponents <-
    App.Sql.query
      "select * from packageMetaComponent where packageMeta = ? and component = ? limit 1"
      (packageMeta, component)
  NotFound.fromList packageMetaComponents

getPackageMetaComponentModule :: PackageMetaComponent.Key -> Module.Key -> App.App PackageMetaComponentModule.Model
getPackageMetaComponentModule packageMetaComponent module_ = do
  packageMetaComponentModules <-
    App.Sql.query
      "select * from packageMetaComponentModule where packageMetaComponent = ? and module = ? limit 1"
      (packageMetaComponent, module_)
  NotFound.fromList packageMetaComponentModules

getUpload :: Package.Key -> Version.Key -> Revision.Revision -> App.App Upload.Model
getUpload package version revision = do
  uploads <-
    App.Sql.query
      "select * from upload where package = ? and version = ? and revision = ? limit 1"
      (package, version, revision)
  NotFound.fromList uploads

getVersion :: VersionNumber.VersionNumber -> App.App Version.Model
getVersion versionNumber = do
  versions <-
    App.Sql.query
      "select * from version where number = ? limit 1"
      [versionNumber]
  NotFound.fromList versions
