module Monadoc.Handler.Package.Get where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Database.SQLite.Simple as Sql
import qualified Lucid
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Handler.Home.Get as Home.Get
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.PreferredVersions as PreferredVersions
import qualified Monadoc.Model.Release as Release
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Type.Constraint as Constraint
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

handler :: (MonadSql.MonadSql m, Exception.MonadThrow m) => PackageName.PackageName -> Wai.Request -> m Wai.Response
handler packageName _ = do
  package <- do
    rows <- MonadSql.query "select * from package where name = ?" [packageName]
    case rows of
      [] -> Exception.throwM NotFound.NotFound
      row : _ -> pure row
  constraint <- do
    rows <- MonadSql.query "select * from preferredVersions where package = ?" [Model.key package]
    case rows of
      [] -> pure Constraint.any
      row : _ -> pure . PreferredVersions.constraint $ Model.value row
  rows <-
    MonadSql.query
      "select * \
      \ from release \
      \ inner join version \
      \ on version.key = release.version \
      \ inner join hackageUser \
      \ on hackageUser.key = release.uploadedBy \
      \ where release.package = ? \
      \ order by release.uploadedAt desc"
      [Model.key package]
  pure . Home.Get.htmlResponse Http.ok200 [] $ do
    Lucid.doctype_
    Lucid.html_ $ do
      Lucid.body_ $ do
        Lucid.p_ . Lucid.toHtml . Package.name $ Model.value package
        Lucid.p_ $ do
          "Preferred versions: "
          Lucid.toHtml constraint
        Lucid.ul_ . Monad.forM_ rows $ \row -> Lucid.li_ $ do
          let (release Sql.:. version Sql.:. hackageUser) = row
          "Version "
          Lucid.toHtml . Version.number $ Model.value version
          " revision "
          Lucid.toHtml . Release.revision $ Model.value release
          " released at "
          Lucid.toHtml . Release.uploadedAt $ Model.value release
          " by "
          Lucid.toHtml . HackageUser.name $ Model.value hackageUser
          "."
