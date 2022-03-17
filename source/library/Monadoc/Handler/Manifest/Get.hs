{-# LANGUAGE TypeApplications #-}

module Monadoc.Handler.Manifest.Get where

import qualified Data.Aeson as Aeson
import qualified Monadoc.Constant.ContentType as ContentType
import qualified Monadoc.Handler.Home.Get as Home.Get
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Vendor.Witch as Witch
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

handler :: Wai.Request -> App.App Wai.Response
handler _ = do
  context <- App.ask
  pure
    . Wai.responseLBS Http.ok200 [(Http.hContentType, ContentType.manifest)]
    . Aeson.encode
    $ Aeson.object
      [ pair "$schema" "https://json.schemastore.org/web-manifest-combined.json",
        pair "name" "Monadoc",
        pair "start_url" . Config.base $ Context.config context,
        pair "display" "minimal-ui",
        pair
          "icons"
          [ Aeson.object
              [ pair "sizes" "192x192",
                pair "purpose" "any maskable",
                pair "src" $ Home.Get.route context Route.AppleTouchIcon,
                pair "type" "image/png"
              ],
            Aeson.object
              [ pair "sizes" "512x512",
                pair "purpose" "any maskable",
                pair "src" $ Home.Get.route context Route.AppleTouchIcon,
                pair "type" "image/png"
              ]
          ],
        pair "background_color" "#5e2ca5",
        pair "theme_color" "#5e2ca5"
      ]

pair :: (Aeson.ToJSON v, Aeson.KeyValue p) => String -> v -> p
pair = (Aeson..=) . Witch.into @Aeson.Key
