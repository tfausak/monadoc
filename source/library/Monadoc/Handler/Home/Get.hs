module Monadoc.Handler.Home.Get where

import qualified Control.Monad as Monad
import qualified Control.Monad.Reader as Reader
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Lucid
import qualified Lucid.Base as Lucid
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Constant.ContentType as ContentType
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.Release as Release
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.VersionNumber as VersionNumber
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Paths_monadoc as Monadoc
import qualified Witch

handler :: (Reader.MonadReader Context.Context m, MonadSql.MonadSql m) => Wai.Request -> m Wai.Response
handler _ = do
  context <- Reader.ask
  rows <-
    MonadSql.query_
      "select * \
      \ from release \
      \ inner join package \
      \ on package.key = release.package \
      \ inner join version \
      \ on version.key = release.version \
      \ inner join hackageUser \
      \ on hackageUser.key = release.uploadedBy \
      \ order by release.uploadedAt desc \
      \ limit 16"
  pure . htmlResponse Http.ok200 [] $ do
    Lucid.doctype_
    Lucid.html_ [Lucid.lang_ "en-US"] $ do
      Lucid.head_ $ do
        let title = "Monadoc" :: Text.Text
            description = "Worse Haskell documentation." :: Text.Text
            url = route context Route.Home
        Lucid.meta_ [Lucid.charset_ "utf-8"]
        Lucid.meta_ [Lucid.name_ "viewport", Lucid.content_ "initial-scale = 1, width = device-width"]
        Lucid.meta_ [Lucid.name_ "description", Lucid.content_ description]
        Lucid.title_ $ Lucid.toHtml title
        Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.href_ $ route context Route.Bootstrap]
        Lucid.link_ [Lucid.rel_ "manifest", Lucid.href_ $ route context Route.Manifest]
        Lucid.link_ [Lucid.rel_ "canonical", Lucid.href_ url]
        og "title" title
        og "type" "website"
        og "url" url
        og "image" $ route context Route.AppleTouchIcon
        og "description" description
      Lucid.body_ $ do
        Lucid.div_ [Lucid.class_ "bg-dark navbar navbar-dark"] $ do
          Lucid.div_ [Lucid.class_ "container"] $ do
            Lucid.a_ [Lucid.class_ "navbar-brand", Lucid.href_ $ route context Route.Home] "Monadoc"
        Lucid.div_ [Lucid.class_ "my-3"] $ do
          Lucid.div_ [Lucid.class_ "container"] $ do
            Lucid.h2_ "Recent Releases"
            Lucid.ul_ [] $ do
              Monad.forM_ rows $ \row -> Lucid.li_ [] $ do
                let (release Sql.:. package Sql.:. version Sql.:. hackageUser) = row
                "Package "
                Lucid.a_
                  [Lucid.href_ . route context . Route.Package . Package.name $ Model.value package]
                  . Lucid.toHtml
                  . Package.name
                  $ Model.value package
                " version "
                Lucid.toHtml . Version.number $ Model.value version
                " revision "
                Lucid.toHtml . Release.revision $ Model.value release
                " released at "
                Lucid.toHtml . Release.uploadedAt $ Model.value release
                " by "
                Lucid.toHtml . HackageUser.name $ Model.value hackageUser
                "."
        Lucid.div_ [Lucid.class_ "my-3 text-muted"] $ do
          Lucid.div_ [Lucid.class_ "border-top container pt-3"] $ do
            "Powered by "
            Lucid.a_ [Lucid.href_ "https://github.com/tfausak/monadoc"] "Monadoc"
            " version "
            Lucid.toHtml $ Witch.into @VersionNumber.VersionNumber Monadoc.version
            ". \x1f516"

og :: Text.Text -> Text.Text -> Lucid.Html ()
og property content =
  Lucid.meta_
    [ Lucid.makeAttribute "property" $ "og:" <> property,
      Lucid.content_ content
    ]

route :: Context.Context -> Route.Route -> Text.Text
route context =
  mappend (Text.pack . Config.base $ Context.config context)
    . Text.intercalate "/"
    . Route.render

htmlResponse :: Http.Status -> [Http.Header] -> Lucid.Html a -> Wai.Response
htmlResponse status headers html =
  Wai.responseLBS status ((Http.hContentType, ContentType.html) : headers) $
    Lucid.renderBS html
