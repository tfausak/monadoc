{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Type.Icon where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Monadoc.Extra.Aeson as Aeson

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
      [ Aeson.pair "sizes" $ sizes icon,
        Aeson.pair "purpose" $ purpose icon,
        Aeson.pair "src" $ src icon,
        Aeson.pair "type" $ type_ icon
      ]
