{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.ComponentType where

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Lucid as Html
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

data ComponentType
  = Benchmark
  | Executable
  | ForeignLibrary
  | Library
  | TestSuite
  deriving (Eq, Show)

instance Witch.TryFrom String ComponentType where
  tryFrom = Witch.maybeTryFrom $ \string -> case string of
    "bench" -> Just Benchmark
    "exe" -> Just Executable
    "flib" -> Just ForeignLibrary
    "lib" -> Just Library
    "test" -> Just TestSuite
    _ -> Nothing

instance Witch.From ComponentType String where
  from componentType = case componentType of
    Benchmark -> "bench"
    Executable -> "exe"
    ForeignLibrary -> "flib"
    Library -> "lib"
    TestSuite -> "test"

instance Sql.FromField ComponentType where
  fromField field = do
    string <- Sql.fromField @String field
    either (Sql.returnError Sql.ConversionFailed field . show) pure $
      Witch.tryFrom string

instance Sql.ToField ComponentType where
  toField = Sql.toField . Witch.into @String

instance QuickCheck.Arbitrary ComponentType where
  arbitrary =
    QuickCheck.elements
      [ Benchmark,
        Executable,
        ForeignLibrary,
        Library,
        TestSuite
      ]

instance Html.ToHtml ComponentType where
  toHtml = Html.toHtml . Witch.into @String
  toHtmlRaw = Html.toHtmlRaw . Witch.into @String
