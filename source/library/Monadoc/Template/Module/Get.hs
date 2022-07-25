module Monadoc.Template.Module.Get where

import qualified Lucid as Html
import qualified Monadoc.Template.Common as Common
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Route as Route

render ::
  Context.Context ->
  Route.Route ->
  [Breadcrumb.Breadcrumb] ->
  Html.Html ()
render context route breadcrumbs = do
  Common.base context route breadcrumbs "TODO" $ pure ()
