{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.Query where

import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Lucid
import qualified Witch

newtype Query
  = Query Text.Text
  deriving (Eq, Show)

instance Witch.From Text.Text Query

instance Witch.From Query Text.Text

instance Witch.From Query ByteString.ByteString where
  from = Witch.via @Text.Text

instance Witch.TryFrom ByteString.ByteString Query where
  tryFrom = Witch.eitherTryFrom $ fmap (Witch.from @Text.Text) . Text.decodeUtf8'

instance Lucid.ToHtml Query where
  toHtml = Lucid.toHtml . Witch.into @Text.Text
  toHtmlRaw = Lucid.toHtmlRaw . Witch.into @Text.Text

isBlank :: Query -> Bool
isBlank = Text.all Char.isSpace . Witch.into @Text.Text
