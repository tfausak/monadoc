{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Monadoc.Template.User.Get where

import qualified Control.Monad as Monad
import qualified Database.SQLite.Simple as Sql
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

render ::
  Context.Context ->
  HackageUser.Model ->
  [Upload.Model Sql.:. Version.Model Sql.:. Package.Model] ->
  [Package.Model] ->
  Lucid.Html ()
render context hackageUser rows packages = Common.base context (Route.User . HackageUser.name $ Model.value hackageUser) $ do
  Lucid.h2_ . Lucid.toHtml . HackageUser.name $ Model.value hackageUser
  Lucid.h3_ "Uploads"
  Lucid.ul_ . Monad.forM_ rows $ \row -> Lucid.li_ $ do
    let (upload Sql.:. version Sql.:. package) = row
        reversion = Reversion.Reversion {Reversion.revision = Just . Upload.revision $ Model.value upload, Reversion.version = Version.number $ Model.value version}
    Lucid.a_ [Lucid.href_ . Common.route context $ Route.Version (Package.name $ Model.value package) reversion] $ do
      Lucid.toHtml . Package.name $ Model.value package
      "@"
      Lucid.toHtml reversion
    " uploaded "
    Common.timestamp . Upload.uploadedAt $ Model.value upload
    "."
  Lucid.h3_ "Packages"
  Lucid.ul_ . Monad.forM_ packages $ \package ->
    Lucid.li_
      . Lucid.a_ [Lucid.href_ . Common.route context . Route.Package . Package.name $ Model.value package]
      . Lucid.toHtml
      . Package.name
      $ Model.value package
