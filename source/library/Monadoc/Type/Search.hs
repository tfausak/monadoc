module Monadoc.Type.Search where

import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Lucid as Html
import qualified Witch
import qualified Witch.Encoding as Witch

newtype Search
  = Search Text.Text
  deriving (Eq, Show)

instance Witch.From Text.Text Search

instance Witch.From Search Text.Text

instance Witch.From Search ByteString.ByteString where
  from = Witch.via @(Witch.UTF_8 ByteString.ByteString) . Witch.into @Text.Text

instance Witch.TryFrom ByteString.ByteString Search where
  tryFrom = Witch.eitherTryFrom $ fmap (Witch.from @Text.Text) . Text.decodeUtf8'

instance Html.ToHtml Search where
  toHtml = Html.toHtml . Witch.into @Text.Text
  toHtmlRaw = Html.toHtmlRaw . Witch.into @Text.Text

isBlank :: Search -> Bool
isBlank = Text.all Char.isSpace . Witch.into @Text.Text

empty :: Search
empty = Witch.from Text.empty
