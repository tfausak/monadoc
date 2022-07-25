module Monadoc.Handler.Module.Get where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Template.Module.Get as Template
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.ComponentId as ComponentId
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.ModuleName as ModuleName
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Reversion as Reversion
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http

handler ::
  PackageName.PackageName ->
  Reversion.Reversion ->
  ComponentId.ComponentId ->
  ModuleName.ModuleName ->
  Handler.Handler
handler _ _ _ _ _ respond = do
  context <- Reader.ask
  -- TODO
  let route = Route.Home
      breadcrumbs = [Breadcrumb.Breadcrumb {Breadcrumb.label = "Home", Breadcrumb.route = Just Route.Home}]
  respond
    . Common.htmlResponse Http.ok200 []
    $ Template.render context route breadcrumbs
