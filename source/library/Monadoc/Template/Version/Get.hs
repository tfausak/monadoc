module Monadoc.Template.Version.Get where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Void as Void
import qualified Database.SQLite.Simple as Sql
import qualified Documentation.Haddock.Markup as Haddock
import qualified Documentation.Haddock.Parser as Haddock
import qualified Documentation.Haddock.Types as Haddock
import qualified Formatting as F
import qualified Lucid as Html
import qualified Monadoc.Extra.Ord as Ord
import qualified Monadoc.Handler.Proxy.Get as Proxy.Get
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Model.PackageMetaComponent as PackageMetaComponent
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Template.Common as Common
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.ComponentId as ComponentId
import qualified Monadoc.Type.ComponentType as ComponentType
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Reversion as Reversion
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.Url as Url
import qualified Witch

data Input = Input
  { breadcrumbs :: [Breadcrumb.Breadcrumb],
    package :: Package.Model,
    version :: Version.Model,
    upload :: Upload.Model,
    hackageUser :: HackageUser.Model,
    maybeLatest :: Maybe (Upload.Model, Version.Model),
    packageMeta :: PackageMeta.Model,
    components :: [PackageMetaComponent.Model Sql.:. Component.Model]
  }
  deriving (Eq, Show)

render :: Context.Context -> Input -> Html.Html ()
render context input = do
  let packageName = input.package.value.name
      versionNumber = input.version.value.number
      revision = input.upload.value.revision
      reversion =
        Reversion.Reversion
          { Reversion.revision = revision,
            Reversion.version = versionNumber
          }
      route = Route.Version packageName reversion
      title =
        F.sformat
          ("Package" F.%+ F.stext F.%+ "version" F.%+ F.stext F.%+ ":: Monadoc")
          (Witch.from packageName)
          (Witch.from reversion)
  Common.base context route input.breadcrumbs title $ do
    showDeprecationWarning packageName reversion input.upload
    showLatestInfo context packageName input.maybeLatest $ const Nothing
    Html.h2_ $ Html.toHtml packageName
    Html.p_ $ do
      "Version "
      Html.toHtml versionNumber
      " revision "
      Html.toHtml revision
      " uploaded "
      Common.timestamp input.upload.value.uploadedAt
      " by "
      Html.a_ [Html.href_ . Common.route context $ Route.User input.hackageUser.value.name] $
        Html.toHtml input.hackageUser.value.name
      "."
    Html.h3_ "Package meta"
    Html.dl_ $ do
      Html.dt_ "Synopsis"
      Html.dd_ . maybe "n/a" Html.toHtml $ input.packageMeta.value.synopsis
      Html.dt_ "Description"
      Html.dd_
        . Html.toHtml
        . Haddock.markup (markup context)
        . Haddock.overIdentifier (curry Just)
        . Haddock._doc
        . Haddock.parseParas Nothing
        $ maybe "" (Witch.into @String) input.packageMeta.value.description
      Html.dt_ "Author"
      Html.dd_ . maybe "n/a" Html.toHtml $ input.packageMeta.value.author
      Html.dt_ "Bug reports"
      Html.dd_ . maybe "n/a" autoLinkUrl $ input.packageMeta.value.bugReports
      Html.dt_ "Category"
      Html.dd_ . maybe "n/a" Html.toHtml $ input.packageMeta.value.category
      Html.dt_ "Copyright"
      Html.dd_ . maybe "n/a" Html.toHtml $ input.packageMeta.value.copyright
      Html.dt_ "Homepage"
      Html.dd_ . maybe "n/a" autoLinkUrl $ input.packageMeta.value.homepage
      Html.dt_ "Maintainer"
      Html.dd_ . maybe "n/a" Html.toHtml $ input.packageMeta.value.maintainer
      Html.dt_ "Package URL"
      Html.dd_ . maybe "n/a" autoLinkUrl $ input.packageMeta.value.pkgUrl
      Html.dt_ "Stability"
      Html.dd_ . maybe "n/a" Html.toHtml $ input.packageMeta.value.stability
    Html.h3_ "Components"
    Html.ul_ . Monad.forM_ (sortComponents packageName $ fmap (\(_ Sql.:. c) -> c) input.components) $ \component -> Html.li_ $ do
      let componentId =
            ComponentId.ComponentId
              { ComponentId.type_ = component.value.type_,
                ComponentId.name = component.value.name
              }
      Html.a_ [Html.href_ . Common.route context $ Route.Component packageName reversion componentId] $
        Html.toHtml componentId

autoLinkUrl :: Text.Text -> Html.Html ()
autoLinkUrl text =
  case Witch.tryInto @Url.Url text of
    Left _ -> Html.toHtml text
    Right url ->
      Html.a_
        [ Html.href_ $ Witch.into @Text.Text url,
          Html.rel_ "nofollow"
        ]
        . Html.toHtml
        $ Witch.into @Text.Text url

showDeprecationWarning :: PackageName.PackageName -> Reversion.Reversion -> Upload.Model -> Html.Html ()
showDeprecationWarning pkg rev upl = do
  Monad.when (not upl.value.isPreferred)
    . Html.div_ [Html.class_ "alert alert-warning"]
    $ do
      "Version "
      Html.toHtml rev
      " of "
      Html.toHtml pkg
      " is deprecated."

showLatestInfo ::
  Context.Context ->
  PackageName.PackageName ->
  Maybe (Upload.Model, Version.Model) ->
  (Reversion.Reversion -> Maybe Route.Route) ->
  Html.Html ()
showLatestInfo context packageName m makeRoute =
  case m of
    Nothing -> pure ()
    Just (upl, ver) -> Html.div_ [Html.class_ "alert alert-info"] $ do
      "The latest version of "
      Html.toHtml packageName
      " is "
      let reversion =
            Reversion.Reversion
              { Reversion.version = ver.value.number,
                Reversion.revision = upl.value.revision
              }
          route = Maybe.fromMaybe (Route.Version packageName reversion) $ makeRoute reversion
      Html.a_
        [ Html.class_ "alert-link",
          Html.href_ $ Common.route context route
        ]
        $ Html.toHtml reversion
      "."

sortComponents :: PackageName.PackageName -> [Component.Model] -> [Component.Model]
sortComponents packageName = List.sortOn $ \component ->
  let it = component.value
   in ( it.type_ /= ComponentType.Library || it.name /= Witch.from packageName,
        Witch.into @String it.type_,
        Witch.into @String it.name
      )

markup :: Context.Context -> Haddock.DocMarkupH Void.Void (Haddock.Namespace, String) (Html.Html ())
markup context =
  Haddock.Markup
    { Haddock.markupAName = \x -> Html.a_ [Html.name_ $ Witch.from x] mempty,
      Haddock.markupAppend = mappend,
      Haddock.markupBold = Html.strong_,
      Haddock.markupCodeBlock = Html.pre_ . Html.code_,
      Haddock.markupDefList = Html.dl_ . mapM_ (\(k, v) -> Html.dt_ k <> Html.dd_ v),
      Haddock.markupEmphasis = Html.em_,
      Haddock.markupEmpty = mempty,
      Haddock.markupExample = mapM_ $ \x ->
        Html.pre_
          . Html.code_
          . Html.toHtml
          . unlines
          $ (">>> " <> Haddock.exampleExpression x) : Haddock.exampleResult x,
      Haddock.markupHeader = \x -> case Ord.clamp 1 6 $ Haddock.headerLevel x of
        1 -> Html.h1_ $ Haddock.headerTitle x
        2 -> Html.h2_ $ Haddock.headerTitle x
        3 -> Html.h3_ $ Haddock.headerTitle x
        4 -> Html.h4_ $ Haddock.headerTitle x
        5 -> Html.h5_ $ Haddock.headerTitle x
        _ -> Html.h6_ $ Haddock.headerTitle x,
      Haddock.markupHyperlink = \x ->
        Html.a_
          [ Html.href_ . Witch.from $ Haddock.hyperlinkUrl x,
            Html.rel_ "nofollow"
          ]
          . Maybe.fromMaybe (Html.toHtml $ Haddock.hyperlinkUrl x)
          $ Haddock.hyperlinkLabel x,
      Haddock.markupIdentifier = \(namespace, identifier) ->
        Html.a_
          [Html.name_ . Witch.from $ Haddock.renderNs namespace <> identifier]
          $ Html.toHtml identifier,
      Haddock.markupIdentifierUnchecked = Void.absurd,
      Haddock.markupMathDisplay = \x ->
        Html.span_ [Html.class_ "mathjax"] . Html.toHtml $ "\\[" <> x <> "\\]",
      Haddock.markupMathInline = \x ->
        Html.span_ [Html.class_ "mathjax"] . Html.toHtml $ "\\(" <> x <> "\\)",
      Haddock.markupModule = \x ->
        -- TODO: Search for module specifically.
        Html.a_ [Html.href_ . Common.route context . Route.Search . Witch.via @Text.Text $ Haddock.modLinkName x]
          . Maybe.fromMaybe (Html.toHtml $ Haddock.modLinkName x)
          $ Haddock.modLinkLabel x,
      Haddock.markupMonospaced = Html.code_,
      Haddock.markupOrderedList = Html.ol_ . mapM_ (\(i, x) -> Html.li_ [Html.value_ . Witch.from $ show i] x),
      Haddock.markupParagraph = Html.p_,
      Haddock.markupPic = \x ->
        Html.img_
          [ Html.alt_ . maybe "" Witch.from $ Haddock.pictureTitle x,
            Html.loading_ "lazy",
            Html.src_ $ case Witch.tryFrom $ Haddock.pictureUri x of
              Left _ ->
                -- TODO: Handle relative paths.
                Witch.from $ Haddock.pictureUri x
              Right url ->
                if Url.isData url
                  then Witch.from url
                  else Common.route context $ Proxy.Get.makeRoute context url
          ],
      Haddock.markupProperty = \x -> Html.pre_ . Html.code_ $ "prop> " <> Html.toHtml x,
      Haddock.markupString = Html.toHtml,
      Haddock.markupTable = \t -> Html.table_ [Html.class_ "table"] $ do
        Html.thead_
          . Monad.forM_ (Haddock.tableHeaderRows t)
          $ \r -> Html.tr_
            . Monad.forM_ (Haddock.tableRowCells r)
            $ \c ->
              Html.th_
                [ Html.colspan_ . Witch.from . show $ Haddock.tableCellColspan c,
                  Html.rowspan_ . Witch.from . show $ Haddock.tableCellRowspan c
                ]
                $ Haddock.tableCellContents c
        Html.tbody_
          . Monad.forM_ (Haddock.tableBodyRows t)
          $ \r -> Html.tr_
            . Monad.forM_ (Haddock.tableRowCells r)
            $ \c ->
              Html.td_
                [ Html.colspan_ . Witch.from . show $ Haddock.tableCellColspan c,
                  Html.rowspan_ . Witch.from . show $ Haddock.tableCellRowspan c
                ]
                $ Haddock.tableCellContents c,
      Haddock.markupUnorderedList = Html.ul_ . mapM_ Html.li_,
      Haddock.markupWarning = Html.div_ [Html.class_ "alert alert-warning"]
    }
