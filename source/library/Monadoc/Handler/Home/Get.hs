{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Handler.Home.Get where

import qualified Control.Monad as Monad
import qualified Control.Monad.Reader as Reader
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Version as Version
import qualified Database.SQLite.Simple as Sql
import qualified Lucid
import qualified Lucid.Base as Lucid
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Constant.ContentType as ContentType
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.HackageUserName as HackageUserName
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Revision as Revision
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
      "select hackageUser.name, package.name, version.number, release.revision, release.uploadedAt \
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
        Lucid.div_ [Lucid.class_ "bg-light mb-3 navbar navbar-light"] $ do
          Lucid.div_ [Lucid.class_ "container"] $ do
            Lucid.ol_ [Lucid.class_ "breadcrumb my-1"] $ do
              Lucid.li_ [Lucid.class_ "active breadcrumb-item"] "Home"
        Lucid.div_ [Lucid.class_ "my-3"] $ do
          Lucid.div_ [Lucid.class_ "container"] $ do
            Lucid.h2_ "Recent Releases"
            Lucid.ul_ [] $ do
              Monad.forM_ rows $ \row -> Lucid.li_ [] $ do
                "Package "
                Lucid.a_
                  [Lucid.href_ . route context . Route.Package $ rowPackage row]
                  . Lucid.toHtml
                  . Witch.into @String
                  $ rowPackage row
                " version "
                Lucid.toHtml . Witch.into @String $ rowVersion row
                " revision "
                Lucid.toHtml . Witch.into @String $ rowRevision row
                " released at "
                Lucid.toHtml . Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" $ rowTime row
                " by "
                Lucid.toHtml . Witch.into @String $ rowUser row
                "."
        Lucid.div_ [Lucid.class_ "my-3 text-muted"] $ do
          Lucid.div_ [Lucid.class_ "border-top container pt-3"] $ do
            "Powered by "
            Lucid.a_ [Lucid.href_ "https://github.com/tfausak/monadoc"] "Monadoc"
            " version "
            Lucid.toHtml $ Version.showVersion Monadoc.version
            ". \x1f516"

data Row = Row
  { rowUser :: HackageUserName.HackageUserName,
    rowPackage :: PackageName.PackageName,
    rowVersion :: VersionNumber.VersionNumber,
    rowRevision :: Revision.Revision,
    rowTime :: Time.UTCTime
  }
  deriving (Eq, Show)

instance Sql.FromRow Row where
  fromRow =
    Row
      <$> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field

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
