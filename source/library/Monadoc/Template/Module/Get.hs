module Monadoc.Template.Module.Get where

import qualified Formatting as F
import qualified Lucid as Html
import qualified Monadoc.Model.Module as Module
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
  Module.Model ->
  Html.Html ()
render context route breadcrumbs module_ = do
  let moduleName = Module.name $ Model.value module_
      title = F.sformat ("Module " F.% F.stext F.% " :: Monadoc") (Witch.from moduleName)
  Common.base context route breadcrumbs title $ do
    -- TODO: Include identifiers exported by the module. This will require
    -- downloading each package's tarball, finding the sources of each module,
    -- and parsing them with GHC.
    Html.h2_ $ Html.toHtml moduleName
