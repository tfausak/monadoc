{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Template.Version.Get where

import qualified Control.Monad as Monad
import qualified Data.Text as Text
import qualified Lucid
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Template.Common as Common
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Reversion as Reversion
import qualified Monadoc.Type.Route as Route
import qualified Witch

render ::
  Context.Context ->
  Package.Model ->
  Version.Model ->
  Upload.Model ->
  HackageUser.Model ->
  Lucid.Html ()
render context package version upload hackageUser = do
  let packageName = Package.name $ Model.value package
      versionNumber = Version.number $ Model.value version
      revision = Upload.revision $ Model.value upload
      reversion = Reversion.Reversion {Reversion.revision = Just revision, Reversion.version = versionNumber}
      route = Route.Version packageName reversion
      title = "Package " <> Witch.into @Text.Text packageName <> " version " <> Witch.into @Text.Text reversion <> " :: Monadoc"
  Common.base context route title $ do
    Lucid.h2_ . Lucid.a_ [Lucid.href_ . Common.route context . Route.Package . Package.name $ Model.value package] $ Lucid.toHtml packageName
    Lucid.p_ $ do
      "Version "
      Lucid.toHtml versionNumber
      " revision "
      Lucid.toHtml revision
      " uploaded "
      Common.timestamp . Upload.uploadedAt $ Model.value upload
      " by "
      Lucid.a_ [Lucid.href_ . Common.route context . Route.User . HackageUser.name $ Model.value hackageUser]
        . Lucid.toHtml
        . HackageUser.name
        $ Model.value hackageUser
      "."
      Monad.when (not . Upload.isPreferred $ Model.value upload) " (deprecated)"
