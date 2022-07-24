{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Handler.Component.Get where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Query.Component as Component
import qualified Monadoc.Query.Package as Package
import qualified Monadoc.Query.PackageMeta as PackageMeta
import qualified Monadoc.Query.PackageMetaComponent as PackageMetaComponent
import qualified Monadoc.Query.Upload as Upload
import qualified Monadoc.Query.Version as Version
import qualified Monadoc.Template.Component.Get as Template
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
  package <- do
    maybePackage <- Package.selectByName packageName
    maybe (Traced.throw NotFound.NotFound) pure maybePackage
  version <- do
    maybeVersion <- Version.selectByNumber $ Reversion.version reversion
    maybe (Traced.throw NotFound.NotFound) pure maybeVersion
  upload <- do
    maybeUpload <-
      Upload.selectByPackageAndVersionAndRevision
        (Model.key package)
        (Model.key version)
        (Reversion.revision reversion)
    maybe (Traced.throw NotFound.NotFound) pure maybeUpload
  packageMeta <- do
    maybePackageMeta <- PackageMeta.selectByUpload $ Model.key upload
    maybe (Traced.throw NotFound.NotFound) pure maybePackageMeta
  component <- do
    maybeComponent <-
      Component.selectByTypeAndName
        (ComponentId.type_ componentId)
        (ComponentId.name componentId)
    maybe (Traced.throw NotFound.NotFound) pure maybeComponent
  packageMetaComponent <- do
    maybePackageMetaComponent <-
      PackageMetaComponent.selectByPackageMetaAndComponent
        (Model.key packageMeta)
        (Model.key component)
    maybe (Traced.throw NotFound.NotFound) pure maybePackageMetaComponent
  packageMetaComponentModules <-
    App.Sql.query
      "select * \
      \ from packageMetaComponentModule \
      \ inner join module \
      \ on module.key = packageMetaComponentModule.module \
      \ where packageMetaComponentModule.packageMetaComponent = ?"
      [Model.key packageMetaComponent]
  context <- Reader.ask
  let breadcrumbs =
        [ Breadcrumb.Breadcrumb {Breadcrumb.label = "Home", Breadcrumb.route = Just Route.Home},
          Breadcrumb.Breadcrumb {Breadcrumb.label = Witch.from packageName, Breadcrumb.route = Just $ Route.Package packageName},
          Breadcrumb.Breadcrumb {Breadcrumb.label = Witch.from reversion, Breadcrumb.route = Just $ Route.Version packageName reversion},
          Breadcrumb.Breadcrumb {Breadcrumb.label = Witch.from componentId, Breadcrumb.route = Nothing}
        ]
  respond
    . Common.htmlResponse Http.ok200 []
    $ Template.render context breadcrumbs package version upload packageMeta component packageMetaComponent packageMetaComponentModules
