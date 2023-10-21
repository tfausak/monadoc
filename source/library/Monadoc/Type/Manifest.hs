module Monadoc.Type.Manifest where

import qualified Data.Aeson as Aeson
import qualified Data.Hashable as Hashable
import qualified Data.Text as Text
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
      [ "background_color" Aeson..= manifest.backgroundColor,
        "display" Aeson..= manifest.display,
        "icons" Aeson..= manifest.icons,
        "name" Aeson..= manifest.name,
        "$schema" Aeson..= manifest.schema,
        "start_url" Aeson..= manifest.startUrl,
        "theme_color" Aeson..= manifest.themeColor
      ]

instance Hashable.Hashable Manifest where
  hashWithSalt s x =
    s
      `Hashable.hashWithSalt` x.backgroundColor
      `Hashable.hashWithSalt` x.display
      `Hashable.hashWithSalt` x.icons
      `Hashable.hashWithSalt` x.name
      `Hashable.hashWithSalt` x.schema
      `Hashable.hashWithSalt` x.startUrl
      `Hashable.hashWithSalt` x.themeColor
