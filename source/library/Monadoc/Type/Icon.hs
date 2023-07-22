module Monadoc.Type.Icon where

import qualified Data.Aeson as Aeson
import qualified Data.Hashable as Hashable
import qualified Data.Text as Text

data Icon = Icon
  { sizes :: Text.Text,
    purpose :: Text.Text,
    src :: Text.Text,
    type_ :: Text.Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON Icon where
  toJSON icon =
    Aeson.object
      [ "sizes" Aeson..= sizes icon,
        "purpose" Aeson..= purpose icon,
        "src" Aeson..= src icon,
        "type" Aeson..= type_ icon
      ]

instance Hashable.Hashable Icon where
  hashWithSalt s x =
    s
      `Hashable.hashWithSalt` sizes x
      `Hashable.hashWithSalt` purpose x
      `Hashable.hashWithSalt` src x
      `Hashable.hashWithSalt` type_ x
