{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.PackageName where

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.Package as Cabal
import qualified Lucid as Html
import qualified Monadoc.Extra.Cabal as Cabal
import qualified Monadoc.Extra.Either as Either
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

newtype PackageName
  = PackageName Cabal.PackageName
  deriving (Eq, Ord, Show)

instance Witch.From Cabal.PackageName PackageName

instance Witch.From PackageName Cabal.PackageName

instance Witch.TryFrom String PackageName where
  tryFrom =
    Witch.eitherTryFrom $
      fmap (Witch.from @Cabal.PackageName)
        . Cabal.tryParsec

instance Witch.From PackageName String where
  from = Cabal.unPackageName . Witch.into @Cabal.PackageName

instance Witch.TryFrom Text.Text PackageName where
  tryFrom = Witch.eitherTryFrom $ Witch.tryFrom . Witch.into @String

instance Witch.From PackageName Text.Text where
  from = Witch.via @String

instance Sql.FromField PackageName where
  fromField field = do
    string <- Sql.fromField @String field
    either (Sql.returnError Sql.ConversionFailed field . show) pure $
      Witch.tryFrom string

instance Sql.ToField PackageName where
  toField = Sql.toField . Witch.into @String

instance Html.ToHtml PackageName where
  toHtml = Html.toHtml . Witch.into @String
  toHtmlRaw = Html.toHtmlRaw . Witch.into @String

instance QuickCheck.Arbitrary PackageName where
  arbitrary = QuickCheck.suchThatMap genString $ Either.hush . Witch.tryFrom

-- | Package names are made up of parts separated by hyphens. There must be
-- at least one part.
genString :: QuickCheck.Gen String
genString = List.intercalate "-" <$> QuickCheck.listOf1 genPart

-- | Each part of a package name is made up of one or more alphanumeric
-- characters. There must be at least one non-numeric character in each part.
-- Although Cabal technically allows Unicode, Hackage requires ASCII.
genPart :: QuickCheck.Gen String
genPart = do
  xs <- (:) <$> genAlpha <*> QuickCheck.listOf genAlphaNum
  QuickCheck.shuffle xs

genAlpha :: QuickCheck.Gen Char
genAlpha = QuickCheck.elements $ ['A' .. 'Z'] <> ['a' .. 'z']

genAlphaNum :: QuickCheck.Gen Char
genAlphaNum = QuickCheck.oneof [genAlpha, QuickCheck.elements ['0' .. '9']]
