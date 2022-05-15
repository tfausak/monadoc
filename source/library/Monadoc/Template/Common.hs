{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Template.Common where

import qualified Data.Text as Text
import qualified Lucid
import qualified Lucid.Base as Lucid
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Monadoc.Type.VersionNumber as VersionNumber
import qualified Paths_monadoc as Monadoc
import qualified Witch

base :: Context.Context -> Route.Route -> Text.Text -> Lucid.Html () -> Lucid.Html ()
base ctx rt title html = do
  Lucid.doctype_
  Lucid.html_ [Lucid.lang_ "en-US"] $ do
    Lucid.head_ $ do
      let description = "Worse Haskell documentation." :: Text.Text
          canonical = route ctx rt
      Lucid.meta_ [Lucid.charset_ "utf-8"]
      Lucid.meta_ [Lucid.name_ "viewport", Lucid.content_ "initial-scale = 1, width = device-width"]
      Lucid.meta_ [Lucid.name_ "description", Lucid.content_ description]
      Lucid.title_ $ Lucid.toHtml title
      Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.href_ $ route ctx Route.Stylesheet]
      Lucid.link_ [Lucid.rel_ "manifest", Lucid.href_ $ route ctx Route.Manifest]
      Lucid.link_ [Lucid.rel_ "canonical", Lucid.href_ canonical]
      og "title" title
      og "type" "website"
      og "url" canonical
      og "image" $ route ctx Route.AppleTouchIcon
      og "description" description
    Lucid.body_ $ do
      Lucid.header_ [Lucid.class_ "bg-dark navbar navbar-dark"]
        . Lucid.div_ [Lucid.class_ "container"]
        $ Lucid.a_ [Lucid.class_ "navbar-brand", Lucid.href_ $ route ctx Route.Home] "Monadoc"
      Lucid.main_ [Lucid.class_ "my-3"] $ Lucid.div_ [Lucid.class_ "container"] html
      Lucid.footer_ [Lucid.class_ "my-3 text-muted"]
        . Lucid.div_ [Lucid.class_ "border-top container pt-3"]
        $ do
          "Powered by "
          Lucid.a_ [Lucid.href_ "https://github.com/tfausak/monadoc"] "Monadoc"
          " version "
          Lucid.toHtml $ Witch.into @VersionNumber.VersionNumber Monadoc.version
          case Context.sha ctx of
            Nothing -> ""
            Just sha -> do
              " commit "
              Lucid.a_ [Lucid.href_ $ "https://github.com/tfausak/monadoc/commit/" <> Witch.into @Text.Text sha]
                . Lucid.toHtml
                $ take 7 sha
          ". \x1f516"
      Lucid.script_ [Lucid.src_ $ route ctx Route.Script] Text.empty

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

timestamp :: Timestamp.Timestamp -> Lucid.Html ()
timestamp ts =
  Lucid.abbr_ [Lucid.title_ $ Witch.into @Text.Text ts]
    . Lucid.time_
      [ Lucid.datetime_ $ Witch.into @Text.Text ts,
        Lucid.class_ "timeago"
      ]
    $ Lucid.toHtml ts

url :: Text.Text -> Lucid.Html ()
url x = Lucid.a_ [Lucid.href_ x] $ Lucid.toHtml x
