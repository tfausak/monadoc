{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Monadoc.Template.Package.Get where

import qualified Control.Monad as Monad
import qualified Data.Text as Text
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
import qualified Witch

render ::
  Context.Context ->
  [Breadcrumb.Breadcrumb] ->
  Package.Model ->
  [Upload.Model Sql.:. Version.Model Sql.:. HackageUser.Model] ->
  [HackageUser.Model] ->
  Html.Html ()
render context breadcrumbs package rows hackageUsers = do
  let packageName = Package.name $ Model.value package
      route = Route.Package packageName
      title = "Package " <> Witch.into @Text.Text packageName <> " :: Monadoc"
  Common.base context route breadcrumbs title $ do
    Html.h2_ $ Html.toHtml packageName
    Html.h3_ "Uploads"
    Html.ul_ . Monad.forM_ rows $ \(upload Sql.:. version Sql.:. hackageUser) -> Html.li_ $ do
      let versionNumber = Version.number $ Model.value version
          reversion =
            Reversion.Reversion
              { Reversion.revision = Just . Upload.revision $ Model.value upload,
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
    Html.ul_ . Monad.forM_ hackageUsers $ \hackageUser ->
      Html.li_
        . Html.a_ [Html.href_ . Common.route context . Route.User . HackageUser.name $ Model.value hackageUser]
        . Html.toHtml
        . HackageUser.name
        $ Model.value hackageUser
