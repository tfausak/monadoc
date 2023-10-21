module Monadoc.Handler.Module.Get where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Maybe as Maybe
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Extra.Exception as Exception
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Handler.Component.Get as Component.Get
import qualified Monadoc.Handler.Version.Get as Version.Get
import qualified Monadoc.Model.Module as Module
import qualified Monadoc.Model.PackageMetaComponent as PackageMetaComponent
import qualified Monadoc.Model.PackageMetaComponentModule as PackageMetaComponentModule
import qualified Monadoc.Query.Package as Package.Query
import qualified Monadoc.Query.Version as Version.Query
import qualified Monadoc.Template.Module.Get as Template
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.ComponentId as ComponentId
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.ModuleName as ModuleName
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Reversion as Reversion
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http
import qualified Witch

handler ::
  PackageName.PackageName ->
  Reversion.Reversion ->
  ComponentId.ComponentId ->
  ModuleName.ModuleName ->
  Handler.Handler
handler packageName reversion componentId moduleName _ respond = do
  package <- NotFound.fromMaybe =<< Package.Query.getByName packageName
  version <- NotFound.fromMaybe =<< Version.Query.getByNumber reversion.version
  upload <- Version.Get.getUpload package.key version.key reversion.revision
  packageMeta <- Version.Get.getPackageMeta upload.key
  component <- Component.Get.getComponent componentId
  packageMetaComponent <- Component.Get.getPackageMetaComponent packageMeta.key component.key
  module_ <- getModule moduleName
  Monad.void $ getPackageMetaComponentModule packageMetaComponent.key module_.key
  maybeLatest <- Version.Get.getLatestUpload package.key upload.key
  maybePMC <- case maybeLatest of
    Nothing -> pure Nothing
    Just (u, _) -> Exception.handleIf (Exception.isType @NotFound.NotFound) (const $ pure Nothing) $ do
      pm <- Version.Get.getPackageMeta u.key
      Just <$> Component.Get.getPackageMetaComponent pm.key component.key
  hasModule <- case maybePMC of
    Nothing -> pure False
    Just pmc -> Exception.handleIf (Exception.isType @NotFound.NotFound) (const $ pure False) $ do
      Monad.void $ getPackageMetaComponentModule pmc.key module_.key
      pure True
  context <- Reader.ask
  let breadcrumbs =
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
    $ Template.render
      context
      Template.Input
        { Template.breadcrumbs = breadcrumbs,
          Template.package = package,
          Template.version = version,
          Template.upload = upload,
          Template.maybeLatest = maybeLatest,
          Template.hasComponent = Maybe.isJust maybePMC,
          Template.hasModule = hasModule,
          Template.component = component,
          Template.module_ = module_
        }

getModule :: ModuleName.ModuleName -> App.App Module.Model
getModule moduleName = do
  modules <-
    App.Sql.query
      "select * from module where name = ? limit 1"
      [moduleName]
  NotFound.fromList modules

getPackageMetaComponentModule :: PackageMetaComponent.Key -> Module.Key -> App.App PackageMetaComponentModule.Model
getPackageMetaComponentModule packageMetaComponent module_ = do
  packageMetaComponentModules <-
    App.Sql.query
      "select * from packageMetaComponentModule where packageMetaComponent = ? and module = ? limit 1"
      (packageMetaComponent, module_)
  NotFound.fromList packageMetaComponentModules
