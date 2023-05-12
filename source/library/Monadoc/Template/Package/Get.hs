module Monadoc.Template.Package.Get where

import qualified Control.Monad as Monad
import qualified Database.SQLite.Simple as Sql
import qualified Formatting as F
import qualified Lucid as Html
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.PackageMeta as PackageMeta
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
    package :: Package.Model,
    rows :: [Upload.Model Sql.:. Version.Model Sql.:. HackageUser.Model Sql.:. PackageMeta.Model],
    hackageUsers :: [HackageUser.Model]
  }
  deriving (Eq, Show)

render :: Context.Context -> Input -> Html.Html ()
render context input = do
  let packageName = Package.name . Model.value $ package input
      route = Route.Package packageName
      title = F.sformat ("Package" F.%+ F.stext F.%+ ":: Monadoc") (Witch.from packageName)
  Common.base context route (breadcrumbs input) title $ do
    Html.h2_ $ Html.toHtml packageName
    Html.h3_ "Uploads"
    Html.ul_ . Monad.forM_ (rows input) $ \(upload Sql.:. version Sql.:. hackageUser Sql.:. _) -> Html.li_ $ do
      let versionNumber = Version.number $ Model.value version
          reversion =
            Reversion.Reversion
              { Reversion.revision = Upload.revision $ Model.value upload,
                Reversion.version = versionNumber
              }
      Html.a_ [Html.href_ . Common.route context $ Route.Version packageName reversion] $ do
        Html.toHtml reversion
      " uploaded "
      Common.timestamp . Upload.uploadedAt $ Model.value upload
      " by "
      Html.a_ [Html.href_ . Common.route context . Route.User . HackageUser.name $ Model.value hackageUser]
        . Html.toHtml
        . HackageUser.name
        $ Model.value hackageUser
      "."
      Monad.when (Upload.isLatest $ Model.value upload) $ do
        " "
        Html.span_ [Html.class_ "badge text-bg-info"] "latest"
      Monad.when (not . Upload.isPreferred $ Model.value upload) $ do
        " "
        Html.span_ [Html.class_ "badge text-bg-warning"] "deprecated"
    Html.h3_ "Uploaders"
    Html.ul_ . Monad.forM_ (hackageUsers input) $ \hackageUser ->
      Html.li_
        . Html.a_ [Html.href_ . Common.route context . Route.User . HackageUser.name $ Model.value hackageUser]
        . Html.toHtml
        . HackageUser.name
        $ Model.value hackageUser
