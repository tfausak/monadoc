module Monadoc.Extra.Aeson where

import qualified Data.Aeson as Aeson

pair :: (Aeson.ToJSON v, Aeson.KeyValue p) => Aeson.Key -> v -> p
pair = (Aeson..=)
