module Monadoc.Template.Home.Get where

import qualified Control.Monad as Monad
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Lucid as Html
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Template.Common as Common
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Reversion as Reversion
import qualified Monadoc.Type.Route as Route
import qualified Witch

data Input = Input
  { breadcrumbs :: [Breadcrumb.Breadcrumb],
    rows :: [Upload.Model Sql.:. Package.Model Sql.:. Version.Model Sql.:. HackageUser.Model Sql.:. PackageMeta.Model]
  }
  deriving (Eq, Show)

render :: Context.Context -> Input -> Html.Html ()
render context input =
  Common.base context Route.Home (breadcrumbs input) "Monadoc" $ do
    Html.h2_ "Core Libraries"
    Html.ul_ . Monad.forM_ coreLibraries $ \packageName ->
      Html.li_
        . Html.a_ [Html.href_ . Common.route context $ Route.Package packageName]
        $ Html.toHtml packageName
    Html.h2_ "Recent Uploads"
    Html.ul_ $ do
      Monad.forM_ (rows input) $ \row -> Html.li_ $ do
        let (upload Sql.:. package Sql.:. version Sql.:. hackageUser Sql.:. _) = row
            reversion =
              Reversion.Reversion
                { Reversion.revision = Upload.revision $ Model.value upload,
                  Reversion.version = Version.number $ Model.value version
                }
            packageName = Package.name $ Model.value package
        Html.a_ [Html.href_ . Common.route context $ Route.Version packageName reversion] $ do
          Html.toHtml packageName
          "@"
          Html.toHtml reversion
        " uploaded "
        Common.timestamp . Upload.uploadedAt $ Model.value upload
        " by "
        Html.a_ [Html.href_ . Common.route context . Route.User . HackageUser.name $ Model.value hackageUser]
          . Html.toHtml
          . HackageUser.name
          $ Model.value hackageUser
        "."
        Monad.when (Upload.isLatest $ Model.value upload) $ do
          " "
          Html.span_ [Html.class_ "badge text-bg-info"] "latest"
        Monad.when (not . Upload.isPreferred $ Model.value upload) $ do
          " "
          Html.span_ [Html.class_ "badge text-bg-warning"] "deprecated"

coreLibraries :: [PackageName.PackageName]
coreLibraries =
  fmap
    (Witch.unsafeFrom @Text.Text)
    [ "base",
      "array",
      "binary",
      "bytestring",
      "Cabal-syntax",
      "Cabal",
      "containers",
      "deepseq",
      "directory",
      "exceptions",
      "filepath",
      "ghc-boot-th",
      "ghc-boot",
      "ghc-compact",
      "ghc-heap",
      "ghc-prim",
      "ghc",
      "ghci",
      "haskeline",
      "hpc",
      "integer-gmp",
      "libiserv",
      "mtl",
      "parsec",
      "pretty",
      "process",
      "stm",
      "template-haskell",
      "terminfo",
      "text",
      "time",
      "transformers",
      "unix",
      "Win32",
      "xhtml"
    ]
