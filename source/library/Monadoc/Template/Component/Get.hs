module Monadoc.Template.Component.Get where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Database.SQLite.Simple as Sql
import qualified Formatting as F
import qualified Lucid as Html
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.Dependency as Dependency
import qualified Monadoc.Model.Module as Module
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Model.PackageMetaComponent as PackageMetaComponent
import qualified Monadoc.Model.PackageMetaComponentModule as PackageMetaComponentModule
import qualified Monadoc.Model.Range as Range
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Template.Common as Common
import qualified Monadoc.Template.Version.Get as Version.Get
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.ComponentId as ComponentId
import qualified Monadoc.Type.ComponentName as ComponentName
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Reversion as Reversion
import qualified Monadoc.Type.Route as Route
import qualified Witch

render ::
  Context.Context ->
  [Breadcrumb.Breadcrumb] ->
  Package.Model ->
  Version.Model ->
  Upload.Model ->
  Maybe (Upload.Model, Version.Model) ->
  Bool ->
  PackageMeta.Model ->
  Component.Model ->
  PackageMetaComponent.Model ->
  [PackageMetaComponentModule.Model Sql.:. Module.Model] ->
  [Dependency.Model Sql.:. Package.Model Sql.:. Component.Model Sql.:. Range.Model] ->
  Html.Html ()
render context breadcrumbs package version upload maybeLatest hasComponent _ component _ modules dependencies = do
  let packageName = Package.name $ Model.value package
      reversion =
        Reversion.Reversion
          { Reversion.version = Version.number $ Model.value version,
            Reversion.revision = Upload.revision $ Model.value upload
          }
      componentId =
        ComponentId.ComponentId
          { ComponentId.type_ = Component.type_ $ Model.value component,
            ComponentId.name = Component.name $ Model.value component
          }
      route = Route.Component packageName reversion componentId
      title =
        F.sformat
          ("Package" F.%+ F.stext F.%+ "version" F.%+ F.stext F.%+ "component" F.%+ F.stext F.%+ ":: Monadoc")
          (Witch.from packageName)
          (Witch.from reversion)
          (Witch.from componentId)
  Common.base context route breadcrumbs title $ do
    Version.Get.showDeprecationWarning packageName reversion upload
    Version.Get.showLatestInfo context packageName maybeLatest $ \rev ->
      if hasComponent
        then Just $ Route.Component packageName rev componentId
        else Nothing
    Html.h2_ $ Html.toHtml componentId
    let moduleNames = fmap (\(_ Sql.:. m) -> Module.name $ Model.value m) modules
    Monad.when (not $ null moduleNames) $ do
      Html.h3_ "Modules"
      Html.ul_ . Monad.forM_ (List.sort moduleNames) $ \moduleName -> do
        Html.li_
          . Html.a_ [Html.href_ . Common.route context $ Route.Module packageName reversion componentId moduleName]
          $ Html.toHtml moduleName
    Html.h3_ "Dependencies"
    if null dependencies
      then Html.p_ "None."
      else Html.ul_ . Monad.forM_ (List.sortOn packageAndComponent dependencies) $ \(_ Sql.:. p Sql.:. c Sql.:. r) -> Html.li_ $ do
        let pn = Package.name $ Model.value p
        Html.a_
          [Html.href_ . Common.route context $ Route.Package pn]
          $ Html.toHtml pn
        " "
        let ci =
              ComponentId.ComponentId
                { ComponentId.type_ = Component.type_ $ Model.value c,
                  ComponentId.name = Component.name $ Model.value c
                }
        Html.toHtml ci
        " "
        Html.toHtml . Range.constraint $ Model.value r

packageAndComponent ::
  Dependency.Model Sql.:. Package.Model Sql.:. Component.Model Sql.:. Range.Model ->
  (PackageName.PackageName, ComponentName.ComponentName)
packageAndComponent (_ Sql.:. package Sql.:. component Sql.:. _) =
  ( Package.name $ Model.value package,
    Component.name $ Model.value component
  )
