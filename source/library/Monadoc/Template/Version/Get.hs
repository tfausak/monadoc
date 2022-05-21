{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Monadoc.Template.Version.Get where

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
  Version.Model ->
  Upload.Model ->
  HackageUser.Model ->
  Maybe (Upload.Model Sql.:. Version.Model) ->
  Html.Html ()
render context breadcrumbs package version upload hackageUser maybeLatest = do
  let packageName = Package.name $ Model.value package
      versionNumber = Version.number $ Model.value version
      revision = Upload.revision $ Model.value upload
      reversion = Reversion.Reversion {Reversion.revision = Just revision, Reversion.version = versionNumber}
      route = Route.Version packageName reversion
      title = "Package " <> Witch.into @Text.Text packageName <> " version " <> Witch.into @Text.Text reversion <> " :: Monadoc"
  Common.base context route breadcrumbs title $ do
    case maybeLatest of
      Nothing -> pure ()
      Just (upl Sql.:. ver) -> Html.div_ [Html.class_ "alert alert-info"] $ do
        "The latest version of "
        Html.toHtml packageName
        " is "
        let rev =
              Reversion.Reversion
                { Reversion.version = Version.number $ Model.value ver,
                  Reversion.revision = Just . Upload.revision $ Model.value upl
                }
        Html.a_
          [Html.href_ . Common.route context $ Route.Version packageName rev]
          $ Html.toHtml rev
        "."
    Monad.when (not . Upload.isPreferred $ Model.value upload)
      . Html.div_ [Html.class_ "alert alert-warning"]
      $ do
        "Version "
        Html.toHtml reversion
        " of "
        Html.toHtml packageName
        " is deprecated."
    Html.h2_ $ Html.toHtml packageName
    Html.p_ $ do
      "Version "
      Html.toHtml versionNumber
      " revision "
      Html.toHtml revision
      " uploaded "
      Common.timestamp . Upload.uploadedAt $ Model.value upload
      " by "
      Html.a_ [Html.href_ . Common.route context . Route.User . HackageUser.name $ Model.value hackageUser]
        . Html.toHtml
        . HackageUser.name
        $ Model.value hackageUser
      "."
