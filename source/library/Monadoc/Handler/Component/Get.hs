module Monadoc.Handler.Component.Get where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Maybe as Maybe
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Handler.Common as Common
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
    packages <- App.Sql.query "select * from package where name = ? limit 1" [packageName]
    maybe (Traced.throw NotFound.NotFound) pure $ Maybe.listToMaybe packages
  version <- do
    versions <- App.Sql.query "select * from version where number = ? limit 1" [Reversion.version reversion]
    maybe (Traced.throw NotFound.NotFound) pure $ Maybe.listToMaybe versions
  upload <- do
    uploads <-
      App.Sql.query
        "select * \
        \ from upload \
        \ where package = ? \
        \ and version = ? \
        \ and revision = ? \
        \ limit 1"
        (Model.key package, Model.key version, Reversion.revision reversion)
    maybe (Traced.throw NotFound.NotFound) pure $ Maybe.listToMaybe uploads
  packageMeta <- do
    packageMetas <- App.Sql.query "select * from packageMeta where upload = ? limit 1" [Model.key upload]
    maybe (Traced.throw NotFound.NotFound) pure $ Maybe.listToMaybe packageMetas
  component <- do
    components <- App.Sql.query "select * from component where type = ? and name = ?" (ComponentId.type_ componentId, ComponentId.name componentId)
    maybe (Traced.throw NotFound.NotFound) pure $ Maybe.listToMaybe components
  packageMetaComponent <- do
    packageMetaComponents <-
      App.Sql.query
        "select * \
        \ from packageMetaComponent \
        \ where packageMeta = ? \
        \ and component = ? \
        \ limit 1"
        ( Model.key packageMeta,
          Model.key component
        )
    maybe (Traced.throw NotFound.NotFound) pure $ Maybe.listToMaybe packageMetaComponents
  packageMetaComponentModules <-
    App.Sql.query
      "select * \
      \ from packageMetaComponentModule \
      \ inner join module \
      \ on module.key = packageMetaComponentModule.module \
      \ where packageMetaComponentModule.packageMetaComponent = ?"
      [Model.key packageMetaComponent]
  dependencies <-
    App.Sql.query
      "select * \
      \ from dependency \
      \ inner join package \
      \ on package.key = dependency.package \
      \ inner join component \
      \ on component.key = dependency.component \
      \ inner join range \
      \ on range.key = dependency.range \
      \ where dependency.packageMetaComponent = ?"
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
    $ Template.render context breadcrumbs package version upload packageMeta component packageMetaComponent packageMetaComponentModules dependencies
