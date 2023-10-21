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
      [ "sizes" Aeson..= icon.sizes,
        "purpose" Aeson..= icon.purpose,
        "src" Aeson..= icon.src,
        "type" Aeson..= icon.type_
      ]

instance Hashable.Hashable Icon where
  hashWithSalt s x =
    s
      `Hashable.hashWithSalt` x.sizes
      `Hashable.hashWithSalt` x.purpose
      `Hashable.hashWithSalt` x.src
      `Hashable.hashWithSalt` x.type_
