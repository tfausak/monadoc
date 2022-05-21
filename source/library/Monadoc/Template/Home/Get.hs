{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Monadoc.Template.Home.Get where

import qualified Control.Monad as Monad
import qualified Database.SQLite.Simple as Sql
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

data Input = Input
  { breadcrumbs :: [Breadcrumb.Breadcrumb],
    rows :: [Upload.Model Sql.:. Package.Model Sql.:. Version.Model Sql.:. HackageUser.Model]
  }
  deriving (Eq, Show)

render :: Context.Context -> Input -> Html.Html ()
render context input =
  Common.base context Route.Home (breadcrumbs input) "Monadoc" $ do
    Html.h2_ "Recent Uploads"
    Html.ul_ [] $ do
      Monad.forM_ (rows input) $ \row -> Html.li_ [] $ do
        let (upload Sql.:. package Sql.:. version Sql.:. hackageUser) = row
            reversion =
              Reversion.Reversion
                { Reversion.revision = Just . Upload.revision $ Model.value upload,
                  Reversion.version = Version.number $ Model.value version
                }
            packageName = Package.name $ Model.value package
        Html.a_ [Html.href_ . Common.route context $ Route.Version packageName reversion] $ do
          Html.toHtml packageName
          "@"
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
          Html.span_ [Html.class_ "badge bg-success"] "latest"
        Monad.when (not . Upload.isPreferred $ Model.value upload) $ do
          " "
          Html.span_ [Html.class_ "badge bg-warning text-dark"] "deprecated"
