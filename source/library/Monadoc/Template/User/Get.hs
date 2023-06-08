module Monadoc.Template.User.Get where

import qualified Control.Monad as Monad
import qualified Database.SQLite.Simple as Sql
import qualified Formatting as F
import qualified Lucid as Html
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Template.Common as Common
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Reversion as Reversion
import qualified Monadoc.Type.Route as Route
import qualified Witch

data Input = Input
  { breadcrumbs :: [Breadcrumb.Breadcrumb],
    hackageUser :: HackageUser.Model,
    rows :: [Upload.Model Sql.:. Version.Model Sql.:. Package.Model],
    packages :: [Package.Model]
  }
  deriving (Eq, Show)

render :: Context.Context -> Input -> Html.Html ()
render context input = do
  let hackageUserName = HackageUser.name . Model.value $ hackageUser input
      route = Route.User hackageUserName
      title = F.sformat ("User" F.%+ F.stext F.%+ ":: Monadoc") (Witch.from hackageUserName)
  Common.base context route (breadcrumbs input) title $ do
    Html.h2_ . Html.toHtml . HackageUser.name . Model.value $ hackageUser input
    Html.h3_ "Uploads"
    Html.ul_ . Monad.forM_ (rows input) $ \(upload Sql.:. version Sql.:. package) -> Html.li_ $ do
      let reversion =
            Reversion.Reversion
              { Reversion.revision = Upload.revision $ Model.value upload,
                Reversion.version = Version.number $ Model.value version
              }
      Html.a_ [Html.href_ . Common.route context $ Route.Version (Package.name $ Model.value package) reversion] $ do
        Html.toHtml . Package.name $ Model.value package
        "@"
        Html.toHtml reversion
      " uploaded "
      Common.timestamp . Upload.uploadedAt $ Model.value upload
      "."
      Monad.when (Upload.isLatest $ Model.value upload) $ do
        " "
        Html.span_ [Html.class_ "badge bg-info-subtle text-info-emphasis"] "latest"
      Monad.when (not . Upload.isPreferred $ Model.value upload) $ do
        " "
        Html.span_ [Html.class_ "badge bg-warning-subtle text-warning-emphasis"] "deprecated"
    Html.h3_ "Packages"
    Html.ul_ . Monad.forM_ (packages input) $ \package ->
      Html.li_
        . Html.a_ [Html.href_ . Common.route context . Route.Package . Package.name $ Model.value package]
        . Html.toHtml
        . Package.name
        $ Model.value package
