module Monadoc.Handler.Manifest.Get where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Aeson as Aeson
import qualified Monadoc.Constant.ContentType as ContentType
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Template.Common as Common
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Icon as Icon
import qualified Monadoc.Type.Manifest as Manifest
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.Wai as Wai

handler :: Handler.Handler
handler _ respond = do
  context <- Reader.ask
  let manifest = makeManifest context
      body = Aeson.encode manifest
      eTag = Common.makeETag manifest
  respond $
    Wai.responseLBS
      Http.ok200
      [ (Http.hCacheControl, "max-age=604800, stale-while-revalidate=86400"),
        (Http.hContentType, ContentType.manifest),
        (Http.hETag, eTag)
      ]
      body

makeManifest :: Context.Context -> Manifest.Manifest
makeManifest context =
  Manifest.Manifest
    { Manifest.schema = "https://json.schemastore.org/web-manifest-combined.json",
      Manifest.name = "Monadoc",
      Manifest.startUrl = context.config.base,
      Manifest.display = "minimal-ui",
      Manifest.icons =
        [ Icon.Icon
            { Icon.sizes = "192x192",
              Icon.purpose = "any maskable",
              Icon.src = Common.route context Route.AppleTouchIcon,
              Icon.type_ = "image/png"
            },
          Icon.Icon
            { Icon.sizes = "512x512",
              Icon.purpose = "any maskable",
              Icon.src = Common.route context Route.AppleTouchIcon,
              Icon.type_ = "image/png"
            }
        ],
      Manifest.backgroundColor = "#ffffff",
      Manifest.themeColor = "#5e2ca5"
    }
