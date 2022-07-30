module Monadoc.Template.Module.Get where

import qualified Data.List as List
import qualified Distribution.ModuleName as Cabal
import qualified Formatting as F
import qualified Lucid as Html
import qualified Monadoc.Model.Module as Module
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Template.Common as Common
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Route as Route
import qualified Witch

render ::
  Context.Context ->
  Route.Route ->
  [Breadcrumb.Breadcrumb] ->
  Package.Model ->
  Version.Model ->
  Module.Model ->
  Html.Html ()
render context route breadcrumbs package version module_ = do
  let moduleName = Module.name $ Model.value module_
      title = F.sformat ("Module " F.% F.stext F.% " :: Monadoc") (Witch.from moduleName)
  Common.base context route breadcrumbs title $ do
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
              (Witch.from . Package.name $ Model.value package)
              (Witch.from . Version.number $ Model.value version)
              (List.intercalate "-" . Cabal.components $ Witch.from moduleName)
      Html.a_ [Html.href_ url] "on Hackage"
      " instead?"
