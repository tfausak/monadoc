{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.Revision where

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Lucid as Html
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

newtype Revision
  = Revision Word
  deriving (Eq, Ord, Show)

instance Witch.From Word Revision

instance Witch.From Revision Word

instance Sql.FromField Revision where
  fromField = fmap (Witch.from @Word) . Sql.fromField

instance Sql.ToField Revision where
  toField = Sql.toField . Witch.into @Word

instance Witch.From Revision String where
  from = show . Witch.into @Word

instance Html.ToHtml Revision where
  toHtml = Html.toHtml . Witch.into @String
  toHtmlRaw = Html.toHtmlRaw . Witch.into @String

instance QuickCheck.Arbitrary Revision where
  arbitrary = Witch.from <$> QuickCheck.arbitrary @Word

zero :: Revision
zero = Witch.from @Word 0

increment :: Revision -> Revision
increment = Witch.over @Word (+ 1)

isZero :: Revision -> Bool
isZero = (==) zero

isNonZero :: Revision -> Bool
isNonZero = not . isZero
