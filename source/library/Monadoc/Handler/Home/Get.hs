{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Handler.Home.Get where

import qualified Data.Text as Text
import qualified Data.Version as Version
import qualified Lucid
import qualified Monadoc.Constant.ContentType as ContentType
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Vendor.Witch as Witch
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Paths_monadoc as Monadoc

handler :: Wai.Request -> App.App Wai.Response
handler _ = do
  context <- App.ask
  pure . htmlResponse Http.ok200 [] $ do
    Lucid.doctype_
    Lucid.html_ [Lucid.lang_ "en-US"] $ do
      Lucid.head_ $ do
        Lucid.meta_ [Lucid.charset_ "utf-8"]
        Lucid.meta_ [Lucid.name_ "viewport", Lucid.content_ "initial-scale = 1, width = device-width"]
        Lucid.title_ "Monadoc"
        Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.href_ $ route context Route.Bootstrap]
        Lucid.link_ [Lucid.rel_ "manifest", Lucid.href_ $ route context Route.Manifest]
        Lucid.link_ [Lucid.rel_ "canonical", Lucid.href_ $ route context Route.Home]
      Lucid.body_ $ do
        Lucid.div_ [Lucid.class_ "bg-dark mb-3 navbar navbar-dark"] $ do
          Lucid.div_ [Lucid.class_ "container"] $ do
            Lucid.a_ [Lucid.class_ "navbar-brand", Lucid.href_ $ route context Route.Home] "Monadoc"
        Lucid.div_ [Lucid.class_ "my-3"] $ do
          Lucid.div_ [Lucid.class_ "container"] $ do
            "TODO"
        Lucid.div_ [Lucid.class_ "my-3 text-muted"] $ do
          Lucid.div_ [Lucid.class_ "border-top container pt-3"] $ do
            "Powered by "
            Lucid.a_ [Lucid.href_ "https://github.com/tfausak/monadoc"] "Monadoc"
            " version "
            Lucid.toHtml $ Version.showVersion Monadoc.version
            ". \x1f516"

route :: Context.Context -> Route.Route -> Text.Text
route context =
  mappend (Witch.into @Text.Text . Config.base $ Context.config context)
    . Text.intercalate "/"
    . Route.render

htmlResponse :: Http.Status -> [Http.Header] -> Lucid.Html a -> Wai.Response
htmlResponse status headers html =
  Wai.responseLBS status ((Http.hContentType, ContentType.html) : headers) $
    Lucid.renderBS html
