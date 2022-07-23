{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.ModuleType where

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Lucid as Html
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

data ModuleType
  = Autogen
  | Exposed
  | Other
  | Virtual
  deriving (Eq, Show)

instance Witch.TryFrom String ModuleType where
  tryFrom = Witch.maybeTryFrom $ \string -> case string of
    "autogen" -> Just Autogen
    "exposed" -> Just Exposed
    "other" -> Just Other
    "virtual" -> Just Virtual
    _ -> Nothing

instance Witch.From ModuleType String where
  from componentType = case componentType of
    Autogen -> "autogen"
    Exposed -> "exposed"
    Other -> "other"
    Virtual -> "virtual"

instance Sql.FromField ModuleType where
  fromField field = do
    string <- Sql.fromField @String field
    either (Sql.returnError Sql.ConversionFailed field . show) pure $
      Witch.tryFrom string

instance Sql.ToField ModuleType where
  toField = Sql.toField . Witch.into @String

instance QuickCheck.Arbitrary ModuleType where
  arbitrary =
    QuickCheck.elements
      [ Autogen,
        Exposed,
        Other,
        Virtual
      ]

instance Html.ToHtml ModuleType where
  toHtml = Html.toHtml . Witch.into @String
  toHtmlRaw = Html.toHtmlRaw . Witch.into @String
