{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Monadoc.Template.Version.Get where

import qualified Control.Monad as Monad
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Lucid as Html
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Model.PackageMetaComponent as PackageMetaComponent
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
  PackageMeta.Model ->
  [PackageMetaComponent.Model Sql.:. Component.Model] ->
  Html.Html ()
render context breadcrumbs package version upload hackageUser maybeLatest packageMeta components = do
  let packageName = Package.name $ Model.value package
      versionNumber = Version.number $ Model.value version
      revision = Upload.revision $ Model.value upload
      reversion = Reversion.Reversion {Reversion.revision = Just revision, Reversion.version = versionNumber}
      route = Route.Version packageName reversion
      title = "Package " <> Witch.into @Text.Text packageName <> " version " <> Witch.into @Text.Text reversion <> " :: Monadoc"
  Common.base context route breadcrumbs title $ do
    Monad.when (not . Upload.isPreferred $ Model.value upload)
      . Html.div_ [Html.class_ "alert alert-warning"]
      $ do
        "Version "
        Html.toHtml reversion
        " of "
        Html.toHtml packageName
        " is deprecated."
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
          [ Html.class_ "alert-link",
            Html.href_ . Common.route context $ Route.Version packageName rev
          ]
          $ Html.toHtml rev
        "."
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
    Html.h3_ "Package meta"
    Html.dl_ $ do
      Html.dt_ "Author"
      Html.dd_ . maybe "n/a" Html.toHtml . PackageMeta.author $ Model.value packageMeta
      Html.dt_ "Bug reports"
      Html.dd_ . maybe "n/a" Html.toHtml . PackageMeta.bugReports $ Model.value packageMeta
      Html.dt_ "Category"
      Html.dd_ . maybe "n/a" Html.toHtml . PackageMeta.category $ Model.value packageMeta
      Html.dt_ "Copyright"
      Html.dd_ . maybe "n/a" Html.toHtml . PackageMeta.copyright $ Model.value packageMeta
      -- TODO: haddock
      Html.dt_ "Description"
      Html.dd_ . maybe "n/a" Html.toHtml . PackageMeta.description $ Model.value packageMeta
      Html.dt_ "Homepage"
      Html.dd_ . maybe "n/a" Html.toHtml . PackageMeta.homepage $ Model.value packageMeta
      Html.dt_ "Maintainer"
      Html.dd_ . maybe "n/a" Html.toHtml . PackageMeta.maintainer $ Model.value packageMeta
      Html.dt_ "Package URL"
      Html.dd_ . maybe "n/a" Html.toHtml . PackageMeta.pkgUrl $ Model.value packageMeta
      Html.dt_ "Stability"
      Html.dd_ . maybe "n/a" Html.toHtml . PackageMeta.stability $ Model.value packageMeta
      Html.dt_ "Synopsis"
      Html.dd_ . maybe "n/a" Html.toHtml . PackageMeta.synopsis $ Model.value packageMeta
    Html.h3_ "Components"
    -- TODO: sort
    Html.ul_ . Monad.forM_ components $ \(_ Sql.:. component) -> Html.li_ $ do
      Html.toHtml . Component.type_ $ Model.value component
      ":"
      Html.toHtml . Component.name $ Model.value component
