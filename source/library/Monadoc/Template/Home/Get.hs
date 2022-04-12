module Monadoc.Template.Home.Get where

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
import qualified Monadoc.Type.Route as Route

render ::
  Context.Context ->
  [Upload.Model Sql.:. Package.Model Sql.:. Version.Model Sql.:. HackageUser.Model] ->
  Lucid.Html ()
render context rows = Common.base context Route.Home $ do
  Lucid.h2_ "Recent Uploads"
  Lucid.ul_ [] $ do
    Monad.forM_ rows $ \row -> Lucid.li_ [] $ do
      let (upload Sql.:. package Sql.:. version Sql.:. hackageUser) = row
      "Package "
      Lucid.a_
        [Lucid.href_ . Common.route context . Route.Package . Package.name $ Model.value package]
        . Lucid.toHtml
        . Package.name
        $ Model.value package
      " version "
      Lucid.toHtml . Version.number $ Model.value version
      " revision "
      Lucid.toHtml . Upload.revision $ Model.value upload
      " uploaded at "
      Lucid.toHtml . Upload.uploadedAt $ Model.value upload
      " by "
      Lucid.toHtml . HackageUser.name $ Model.value hackageUser
      "."
