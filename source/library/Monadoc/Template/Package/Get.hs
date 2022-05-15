{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Monadoc.Template.Package.Get where

import qualified Control.Monad as Monad
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Lucid
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Template.Common as Common
import qualified Monadoc.Type.Constraint as Constraint
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Reversion as Reversion
import qualified Monadoc.Type.Route as Route
import qualified Witch

render ::
  Context.Context ->
  Package.Model ->
  Constraint.Constraint ->
  [Upload.Model Sql.:. Version.Model Sql.:. HackageUser.Model] ->
  [HackageUser.Model] ->
  Lucid.Html ()
render context package constraint rows hackageUsers = do
  let route = Route.Package . Package.name $ Model.value package
      title = "Package " <> (Witch.into @Text.Text . Package.name $ Model.value package) <> " :: Monadoc"
  Common.base context route title $ do
    Lucid.h2_ . Lucid.toHtml . Package.name $ Model.value package
    Lucid.p_ $ do
      "Preferred versions: "
      Lucid.toHtml constraint
    Lucid.h3_ "Uploads"
    Lucid.ul_ . Monad.forM_ rows $ \row -> Lucid.li_ $ do
      let (upload Sql.:. version Sql.:. hackageUser) = row
          versionNumber = Version.number $ Model.value version
          reversion = Reversion.Reversion {Reversion.revision = Just . Upload.revision $ Model.value upload, Reversion.version = versionNumber}
      Lucid.a_ [Lucid.href_ . Common.route context $ Route.Version (Package.name $ Model.value package) reversion] $ do
        Lucid.toHtml reversion
      " uploaded "
      Common.timestamp . Upload.uploadedAt $ Model.value upload
      " by "
      Lucid.a_ [Lucid.href_ . Common.route context . Route.User . HackageUser.name $ Model.value hackageUser]
        . Lucid.toHtml
        . HackageUser.name
        $ Model.value hackageUser
      "."
      Monad.when (Constraint.excludes versionNumber constraint) " (deprecated)"
    Lucid.h3_ "Uploaders"
    Lucid.ul_ . Monad.forM_ hackageUsers $ \hackageUser ->
      Lucid.li_
        . Lucid.a_ [Lucid.href_ . Common.route context . Route.User . HackageUser.name $ Model.value hackageUser]
        . Lucid.toHtml
        . HackageUser.name
        $ Model.value hackageUser
