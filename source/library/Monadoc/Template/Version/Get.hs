module Monadoc.Template.Version.Get where

import qualified Lucid
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Template.Common as Common
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Route as Route

render ::
  Context.Context ->
  Package.Model ->
  Version.Model ->
  Upload.Model ->
  Lucid.Html ()
render context package version upload = do
  let packageName = Package.name $ Model.value package
      versionNumber = Version.number $ Model.value version
      revision = Upload.revision $ Model.value upload
  Common.base context (Route.Version packageName versionNumber) $ do
    Lucid.h2_ . Lucid.a_ [Lucid.href_ . Common.route context . Route.Package . Package.name $ Model.value package] $ Lucid.toHtml packageName
    Lucid.p_ $ do
      "Version "
      Lucid.toHtml versionNumber
      " revision "
      Lucid.toHtml revision
      " uploaded "
      Common.timestamp . Upload.uploadedAt $ Model.value upload
      "."
