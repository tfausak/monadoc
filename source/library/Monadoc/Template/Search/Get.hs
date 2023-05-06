module Monadoc.Template.Search.Get where

import qualified Control.Monad as Monad
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Formatting as F
import qualified Lucid as Html
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Model.Module as Module
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Template.Common as Common
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.ComponentId as ComponentId
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Reversion as Reversion
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.Search as Search
import qualified Witch

render ::
  Context.Context ->
  [Breadcrumb.Breadcrumb] ->
  Search.Search ->
  [Package.Model] ->
  [HackageUser.Model] ->
  [Package.Model Sql.:. Version.Model Sql.:. Upload.Model Sql.:. Component.Model Sql.:. Module.Model] ->
  Html.Html ()
render context breadcrumbs query packages hackageUsers modules = do
  let title = F.sformat ("Search" F.%+ F.stext F.%+ ":: Monadoc") (Witch.from query)
  Common.base context (Route.Search query) breadcrumbs title $ do
    Html.h2_ "Search"
    Html.form_
      [ Html.action_ . Common.route context . Route.Search $ Witch.from @Text.Text "",
        Html.class_ "d-flex mb-3"
      ]
      $ do
        Html.input_
          [ Html.class_ "form-control me-1",
            Html.name_ "query",
            Html.placeholder_ "traverse",
            Html.type_ "search",
            Html.value_ $ Witch.into @Text.Text query
          ]
        Html.button_
          [ Html.class_ "btn btn-primary",
            Html.type_ "submit"
          ]
          "Search"
    Monad.when (not $ Search.isBlank query) $ do
      Html.h3_ "Packages"
      if null packages
        then Html.p_ "None found."
        else Html.ul_ . Monad.forM_ packages $ \package -> do
          let name = Package.name $ Model.value package
          Html.li_
            . Html.a_ [Html.href_ . Common.route context $ Route.Package name]
            $ Html.toHtml name
      Html.h3_ "Modules"
      if null modules
        then Html.p_ "None found."
        else Html.ul_ . Monad.forM_ modules $ \(package Sql.:. version Sql.:. upload Sql.:. component Sql.:. module_) -> do
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
              moduleName = Module.name $ Model.value module_
          Html.li_ $ do
            Html.a_ [Html.href_ . Common.route context $ Route.Module packageName reversion componentId moduleName] $ Html.toHtml moduleName
            " in "
            Html.toHtml componentId
      Html.h3_ "Users"
      if null hackageUsers
        then Html.p_ "None found."
        else Html.ul_ . Monad.forM_ hackageUsers $ \hackageUser -> do
          let name = HackageUser.name $ Model.value hackageUser
          Html.li_
            . Html.a_ [Html.href_ . Common.route context $ Route.User name]
            $ Html.toHtml name
