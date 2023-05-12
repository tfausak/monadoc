module Monadoc.Template.Module.Get where

import qualified Data.List as List
import qualified Distribution.ModuleName as Cabal
import qualified Formatting as F
import qualified Lucid as Html
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.Module as Module
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Template.Common as Common
import qualified Monadoc.Template.Version.Get as Version.Get
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.ComponentId as ComponentId
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Reversion as Reversion
import qualified Monadoc.Type.Route as Route
import qualified Witch

data Input = Input
  { breadcrumbs :: [Breadcrumb.Breadcrumb],
    package :: Package.Model,
    version :: Version.Model,
    upload :: Upload.Model,
    maybeLatest :: Maybe (Upload.Model, Version.Model),
    hasComponent :: Bool,
    hasModule :: Bool,
    component :: Component.Model,
    module_ :: Module.Model
  }
  deriving (Eq, Show)

render :: Context.Context -> Input -> Html.Html ()
render context input = do
  let packageName = Package.name . Model.value $ package input
      reversion =
        Reversion.Reversion
          { Reversion.version = Version.number . Model.value $ version input,
            Reversion.revision = Upload.revision . Model.value $ upload input
          }
      componentId =
        ComponentId.ComponentId
          { ComponentId.type_ = Component.type_ . Model.value $ component input,
            ComponentId.name = Component.name . Model.value $ component input
          }
      moduleName = Module.name . Model.value $ module_ input
      title =
        F.sformat
          ("Package" F.%+ F.stext F.%+ "version" F.%+ F.stext F.%+ "component" F.%+ F.stext F.%+ "module" F.%+ F.stext F.%+ ":: Monadoc")
          (Witch.from packageName)
          (Witch.from reversion)
          (Witch.from componentId)
          (Witch.from moduleName)
      route = Route.Module packageName reversion componentId moduleName
  Common.base context route (breadcrumbs input) title $ do
    Version.Get.showDeprecationWarning packageName reversion $ upload input
    Version.Get.showLatestInfo context packageName (maybeLatest input) $ \rev ->
      if hasModule input
        then Just $ Route.Module packageName rev componentId moduleName
        else
          if hasComponent input
            then Just $ Route.Component packageName rev componentId
            else Nothing
    -- TODO: Include identifiers exported by the module. This will require
    -- downloading each package's tarball, finding the sources of each module,
    -- and parsing them with GHC.
    Html.h2_ $ Html.toHtml moduleName
    Html.p_ $ do
      "Monadoc does not yet provide module details. "
      "Would you like to view this module "
      let url =
            F.sformat
              ("https://hackage.haskell.org/package/" F.% F.stext F.% "-" F.% F.stext F.% "/docs/" F.% F.string F.% ".html")
              (Witch.from packageName)
              (Witch.from . Version.number . Model.value $ version input)
              (List.intercalate "-" . Cabal.components $ Witch.from moduleName)
      Html.a_ [Html.href_ url] "on Hackage"
      " instead?"
