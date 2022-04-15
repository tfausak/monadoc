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
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Route as Route
import qualified Witch

render ::
  Context.Context ->
  Package.Model ->
  Constraint.Constraint ->
  [Upload.Model Sql.:. Version.Model Sql.:. HackageUser.Model] ->
  Lucid.Html ()
render context package constraint rows = Common.base context (Route.Package . Package.name $ Model.value package) $ do
  Lucid.h2_ . Lucid.toHtml . Package.name $ Model.value package
  Lucid.ul_ $ do
    Lucid.li_ . Common.url $
      "https://hackage.haskell.org/package/" <> (Witch.into @Text.Text . Package.name $ Model.value package)
    Lucid.li_ . Common.url $
      "https://www.stackage.org/package/" <> (Witch.into @Text.Text . Package.name $ Model.value package)
  Lucid.p_ $ do
    "Preferred versions: "
    Lucid.toHtml constraint
  Lucid.h3_ "Uploads"
  Lucid.ul_ . Monad.forM_ rows $ \row -> Lucid.li_ $ do
    let (upload Sql.:. version Sql.:. hackageUser) = row
    Lucid.toHtml . Version.number $ Model.value version
    let revision = Upload.revision $ Model.value upload
    Monad.when (Revision.isNonZero revision) $ do
      "-"
      Lucid.toHtml revision
    " uploaded "
    Common.timestamp . Upload.uploadedAt $ Model.value upload
    " by "
    Lucid.a_ [Lucid.href_ . Common.route context . Route.User . HackageUser.name $ Model.value hackageUser]
      . Lucid.toHtml
      . HackageUser.name
      $ Model.value hackageUser
    "."
