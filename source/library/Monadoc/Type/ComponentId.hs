module Monadoc.Type.ComponentId where

import qualified Data.Text as Text
import qualified Lucid as Html
import qualified Monadoc.Extra.Either as Either
import qualified Monadoc.Type.ComponentName as ComponentName
import qualified Monadoc.Type.ComponentType as ComponentType
import qualified Witch

data ComponentId = ComponentId
  { type_ :: ComponentType.ComponentType,
    name :: ComponentName.ComponentName
  }
  deriving (Eq, Ord, Show)

instance Witch.TryFrom String ComponentId where
  tryFrom = Witch.maybeTryFrom $ \string -> do
    let (before, after) = break ((==) ':') string
    type_ <- Either.hush $ Witch.tryFrom before
    name <- Either.hush . Witch.tryFrom $ drop 1 after
    pure ComponentId {type_ = type_, name = name}

instance Witch.TryFrom Text.Text ComponentId where
  tryFrom = Witch.eitherTryFrom $ Witch.tryFrom . Witch.into @String

instance Witch.From ComponentId String where
  from ci = Witch.from ci.type_ <> ":" <> Witch.from ci.name

instance Witch.From ComponentId Text.Text where
  from = Witch.via @String

instance Html.ToHtml ComponentId where
  toHtml = Html.toHtml . Witch.into @String
  toHtmlRaw = Html.toHtmlRaw . Witch.into @String
