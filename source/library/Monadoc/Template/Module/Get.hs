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
  let packageName = input.package.value.name
      reversion =
        Reversion.Reversion
          { Reversion.version = input.version.value.number,
            Reversion.revision = input.upload.value.revision
          }
      componentId =
        ComponentId.ComponentId
          { ComponentId.type_ = input.component.value.type_,
            ComponentId.name = input.component.value.name
          }
      moduleName = input.module_.value.name
      title =
        F.sformat
          ("Package" F.%+ F.stext F.%+ "version" F.%+ F.stext F.%+ "component" F.%+ F.stext F.%+ "module" F.%+ F.stext F.%+ ":: Monadoc")
          (Witch.from packageName)
          (Witch.from reversion)
          (Witch.from componentId)
          (Witch.from moduleName)
      route = Route.Module packageName reversion componentId moduleName
  Common.base context route Nothing input.breadcrumbs title $ do
    Version.Get.showDeprecationWarning packageName reversion input.upload
    Version.Get.showLatestInfo context packageName input.maybeLatest $ \rev ->
      if input.hasModule
        then Just $ Route.Module packageName rev componentId moduleName
        else
          if input.hasComponent
            then Just $ Route.Component packageName rev componentId
            else Nothing
    -- TODO: Include identifiers exported by the module. This will require
    -- downloading each package's tarball, finding the sources of each module,
    -- and parsing them with GHC.
    -- https://github.com/tfausak/monadoc-6/blob/2d7411acffdc2338598f821adf1c0b1d4e4f2ea3/src/lib/Monadoc/Job/FetchDistribution.hs#L28
    -- https://github.com/tfausak/monadoc-5/blob/22a743f6f672b0af7dd75fda932764ca5574ef35/src/lib/Monadoc/Worker/Main.hs#LL574C9-L574C9
    -- https://github.com/tfausak/monadoc-3/blob/90e36c1cccffe6bdb09d798a40d399b61a99cb0a/Main.hs#L499
    -- https://github.com/tfausak/monadoc-2/blob/dac8787a91b245f96e78d528791beb211b567700/source/library/Monadoc.hs#L64
    Html.h2_ $ Html.toHtml moduleName
    Html.p_ $ do
      "Monadoc does not yet provide module details. "
      "Would you like to view this module "
      let url =
            F.sformat
              ("https://hackage.haskell.org/package/" F.% F.stext F.% "-" F.% F.stext F.% "/docs/" F.% F.string F.% ".html")
              (Witch.from packageName)
              (Witch.from input.version.value.number)
              (List.intercalate "-" . Cabal.components $ Witch.from moduleName)
      Html.a_ [Html.href_ url] "on Hackage"
      " instead?"
