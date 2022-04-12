module Monadoc.Handler.Manifest.Get where

import qualified Control.Monad.Reader as Reader
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Monadoc.Constant.ContentType as ContentType
import qualified Monadoc.Extra.Aeson as Aeson
import qualified Monadoc.Template.Common as Common
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
    $ Aeson.encode
      Manifest
        { manifestSchema = "https://json.schemastore.org/web-manifest-combined.json",
          manifestName = "Monadoc",
          manifestStartUrl = Config.base $ Context.config context,
          manifestDisplay = "minimal-ui",
          manifestIcons =
            [ Icon
                { iconSizes = "192x192",
                  iconPurpose = "any maskable",
                  iconSrc = Common.route context Route.AppleTouchIcon,
                  iconType = "image/png"
                },
              Icon
                { iconSizes = "512x512",
                  iconPurpose = "any maskable",
                  iconSrc = Common.route context Route.AppleTouchIcon,
                  iconType = "image/png"
                }
            ],
          manifestBackgroundColor = "#5e2ca5",
          manifestThemeColor = "#5e2ca5"
        }

data Manifest = Manifest
  { manifestSchema :: Text.Text,
    manifestName :: Text.Text,
    manifestStartUrl :: String,
    manifestDisplay :: Text.Text,
    manifestIcons :: [Icon],
    manifestBackgroundColor :: Text.Text,
    manifestThemeColor :: Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Manifest where
  toJSON manifest =
    Aeson.object
      [ Aeson.pair "$schema" $ manifestSchema manifest,
        Aeson.pair "name" $ manifestName manifest,
        Aeson.pair "start_url" $ manifestStartUrl manifest,
        Aeson.pair "display" $ manifestDisplay manifest,
        Aeson.pair "icons" $ manifestIcons manifest,
        Aeson.pair "background_color" $ manifestBackgroundColor manifest,
        Aeson.pair "theme_color" $ manifestThemeColor manifest
      ]

data Icon = Icon
  { iconSizes :: Text.Text,
    iconPurpose :: Text.Text,
    iconSrc :: Text.Text,
    iconType :: Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Icon where
  toJSON icon =
    Aeson.object
      [ Aeson.pair "sizes" $ iconSizes icon,
        Aeson.pair "purpose" $ iconPurpose icon,
        Aeson.pair "src" $ iconSrc icon,
        Aeson.pair "type" $ iconType icon
      ]
