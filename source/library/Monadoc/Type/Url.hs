{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.Url where

import qualified Data.Text as Text
import qualified Network.URI as Uri
import qualified Witch

newtype Url
  = Url Uri.URI
  deriving (Eq, Show)

instance Witch.From Uri.URI Url

instance Witch.From Url Uri.URI

instance Witch.TryFrom String Url where
  tryFrom = Witch.maybeTryFrom $ fmap Witch.from . Uri.parseURI

instance Witch.TryFrom Text.Text Url where
  tryFrom = Witch.eitherTryFrom $ Witch.tryFrom . Witch.into @String

instance Witch.From Url String where
  from = ($ "") . Uri.uriToString id . Witch.from

instance Witch.From Url Text.Text where
  from = Witch.via @String
