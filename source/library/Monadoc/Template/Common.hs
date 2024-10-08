module Monadoc.Template.Common where

import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Lucid as Html
import qualified Lucid.Base as Html
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.Search as Search
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Monadoc.Type.VersionNumber as VersionNumber
import qualified Network.HTTP.Types as Http
import qualified Paths_monadoc as Monadoc
import qualified Witch

base ::
  Context.Context ->
  Route.Route ->
  Maybe Search.Search ->
  [Breadcrumb.Breadcrumb] ->
  Text.Text ->
  Html.Html () ->
  Html.Html ()
base ctx rt maybeQuery breadcrumbs title html = do
  Html.doctype_
  Html.html_ [Html.lang_ "en-US"] $ do
    Html.head_ $ do
      let description = "Worse Haskell documentation." :: Text.Text
          canonical = route ctx rt
      Html.meta_ [Html.charset_ "utf-8"]
      Html.meta_
        [ Html.name_ "viewport",
          Html.content_ "initial-scale = 1, width = device-width"
        ]
      Html.meta_ [Html.name_ "description", Html.content_ description]
      Html.meta_ [Html.name_ "theme-color", Html.content_ "#5e2ca5"]
      Html.title_ $ Html.toHtml title
      Html.link_
        [ Html.rel_ "stylesheet",
          Html.href_ $ route ctx Route.Stylesheet
        ]
      Html.link_ [Html.rel_ "manifest", Html.href_ $ route ctx Route.Manifest]
      Html.link_ [Html.rel_ "canonical", Html.href_ canonical]
      Html.link_ [Html.rel_ "apple-touch-icon", Html.href_ $ route ctx Route.AppleTouchIcon]
      og "title" title
      og "type" "website"
      og "url" canonical
      og "image" $ route ctx Route.AppleTouchIcon
      og "description" description
      Html.script_ [Html.src_ $ route ctx Route.Script] Text.empty
      -- TODO: Implement better handling of static routes.
      Html.script_ [Html.src_ "/static/mathjax/tex-chtml-full.js"] Text.empty
    Html.body_ $ do
      Html.header_ [Html.class_ "bg-dark navbar"]
        . Html.div_ [Html.class_ "container justify-content-start"]
        $ Html.a_
          [ Html.class_ "navbar-brand text-light",
            Html.href_ $ route ctx Route.Home
          ]
          "Monadoc"
      Html.div_ [Html.class_ "bg-body-secondary"]
        . Html.div_ [Html.class_ "container py-2"]
        . Html.form_
          [ Html.action_ . route ctx . Route.Search $ Witch.from @Text.Text "",
            Html.class_ "d-flex"
          ]
        $ do
          Html.input_
            [ Html.class_ "form-control me-1",
              Html.name_ "query",
              Html.placeholder_ "traverse",
              Html.type_ "search",
              Html.value_ . Witch.into @Text.Text $ Maybe.fromMaybe Search.empty maybeQuery
            ]
          Html.button_
            [ Html.class_ "btn btn-outline-primary",
              Html.type_ "submit"
            ]
            "Search"
      Monad.when (not $ null breadcrumbs)
        . Html.nav_ [Html.class_ "bg-body-tertiary"]
        . Html.div_ [Html.class_ "container py-2"]
        . Html.ol_ [Html.class_ "breadcrumb mb-0"]
        . Monad.forM_ breadcrumbs
        $ \breadcrumb -> case breadcrumb.route of
          Just bcrt ->
            Html.li_ [Html.class_ "breadcrumb-item"]
              . Html.a_ [Html.href_ $ route ctx bcrt]
              $ Html.toHtml breadcrumb.label
          Nothing ->
            Html.li_ [Html.class_ "breadcrumb-item active"] $
              Html.toHtml breadcrumb.label
      Html.main_ [Html.class_ "my-3"] $
        Html.div_ [Html.class_ "container"] html
      Html.footer_ [Html.class_ "mb-5 mt-3 text-secondary"]
        . Html.div_ [Html.class_ "border-top container pt-3"]
        $ do
          "Powered by "
          let github = "https://github.com/tfausak/monadoc" :: Text.Text
          Html.a_ [Html.class_ "link-secondary", Html.href_ github] "Monadoc"
          " version "
          Html.toHtml $ Witch.into @VersionNumber.VersionNumber Monadoc.version
          let sha = ctx.config.sha
          if Text.null sha
            then ""
            else do
              " commit "
              Html.code_
                . Html.a_ [Html.class_ "link-secondary", Html.href_ $ github <> "/commit/" <> Witch.into @Text.Text sha]
                . Html.toHtml
                $ Text.take 7 sha
          ". \x1f516"

og :: Text.Text -> Text.Text -> Html.Html ()
og property content =
  Html.meta_
    [ Html.makeAttributes "property" $ "og:" <> property,
      Html.content_ content
    ]

route :: Context.Context -> Route.Route -> Text.Text
route c r =
  let (p, q) = Route.render r
   in mconcat
        [ Text.pack c.config.base,
          Text.intercalate "/" p,
          if null q
            then Text.empty
            else Witch.unsafeInto @Text.Text . Witch.into @(Witch.UTF_8 ByteString.ByteString) $ Http.renderQuery True q
        ]

timestamp :: Timestamp.Timestamp -> Html.Html ()
timestamp ts =
  Html.abbr_ [Html.title_ $ Witch.into @Text.Text ts]
    . Html.time_
      [ Html.datetime_ $ Witch.into @Text.Text ts,
        Html.class_ "relative"
      ]
    $ Html.toHtml ts
