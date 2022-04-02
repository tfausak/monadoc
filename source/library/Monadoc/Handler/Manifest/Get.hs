module Monadoc.Handler.Manifest.Get where

import qualified Control.Monad.Reader as Reader
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Monadoc.Constant.ContentType as ContentType
import qualified Monadoc.Handler.Home.Get as Home.Get
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

handler :: Reader.MonadReader Context.Context m => Wai.Request -> m Wai.Response
handler _ = do
  context <- Reader.ask
  pure
    . Wai.responseLBS Http.ok200 [(Http.hContentType, ContentType.manifest)]
    . Aeson.encode
    $ Aeson.object
      [ pair "$schema" ("https://json.schemastore.org/web-manifest-combined.json" :: Text.Text),
        pair "name" ("Monadoc" :: Text.Text),
        pair "start_url" . Config.base $ Context.config context,
        pair "display" ("minimal-ui" :: Text.Text),
        pair
          "icons"
          [ Aeson.object
              [ pair "sizes" ("192x192" :: Text.Text),
                pair "purpose" ("any maskable" :: Text.Text),
                pair "src" $ Home.Get.route context Route.AppleTouchIcon,
                pair "type" ("image/png" :: Text.Text)
              ],
            Aeson.object
              [ pair "sizes" ("512x512" :: Text.Text),
                pair "purpose" ("any maskable" :: Text.Text),
                pair "src" $ Home.Get.route context Route.AppleTouchIcon,
                pair "type" ("image/png" :: Text.Text)
              ]
          ],
        pair "background_color" ("#5e2ca5" :: Text.Text),
        pair "theme_color" ("#5e2ca5" :: Text.Text)
      ]

pair :: (Aeson.ToJSON v, Aeson.KeyValue p) => Aeson.Key -> v -> p
pair = (Aeson..=)
