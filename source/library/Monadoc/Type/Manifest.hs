module Monadoc.Type.Manifest where

import qualified Data.Aeson as Aeson
import qualified Data.Hashable as Hashable
import qualified Data.Text as Text
import qualified Monadoc.Extra.Aeson as Aeson
import qualified Monadoc.Type.Icon as Icon

data Manifest = Manifest
  { backgroundColor :: Text.Text,
    display :: Text.Text,
    icons :: [Icon.Icon],
    name :: Text.Text,
    schema :: Text.Text,
    startUrl :: String,
    themeColor :: Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Manifest where
  toJSON manifest =
    Aeson.object
      [ Aeson.pair "background_color" $ backgroundColor manifest,
        Aeson.pair "display" $ display manifest,
        Aeson.pair "icons" $ icons manifest,
        Aeson.pair "name" $ name manifest,
        Aeson.pair "$schema" $ schema manifest,
        Aeson.pair "start_url" $ startUrl manifest,
        Aeson.pair "theme_color" $ themeColor manifest
      ]

instance Hashable.Hashable Manifest where
  hashWithSalt s x =
    s
      `Hashable.hashWithSalt` backgroundColor x
      `Hashable.hashWithSalt` display x
      `Hashable.hashWithSalt` icons x
      `Hashable.hashWithSalt` name x
      `Hashable.hashWithSalt` schema x
      `Hashable.hashWithSalt` startUrl x
      `Hashable.hashWithSalt` themeColor x
