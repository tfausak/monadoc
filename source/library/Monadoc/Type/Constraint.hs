module Monadoc.Type.Constraint where

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.Version as Cabal
import qualified Distribution.Types.VersionRange as Cabal
import qualified Lucid as Html
import qualified Monadoc.Extra.Cabal as Cabal
import qualified Monadoc.Type.VersionNumber as VersionNumber
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

newtype Constraint
  = Constraint Cabal.VersionRange
  deriving (Eq, Show)

instance Witch.From Cabal.VersionRange Constraint

instance Witch.From Constraint Cabal.VersionRange

instance Witch.TryFrom String Constraint where
  tryFrom =
    Witch.eitherTryFrom $
      fmap (Witch.from @Cabal.VersionRange)
        . Cabal.tryParsec

instance Witch.From Constraint String where
  from = Cabal.prettyShow . Witch.into @Cabal.VersionRange

instance Sql.FromField Constraint where
  fromField field = do
    string <- Sql.fromField @String field
    either (Sql.returnError Sql.ConversionFailed field . show) pure $
      Witch.tryFrom string

instance Sql.ToField Constraint where
  toField = Sql.toField . Witch.into @String

instance Html.ToHtml Constraint where
  toHtml = Html.toHtml . Witch.into @String
  toHtmlRaw = Html.toHtmlRaw . Witch.into @String

instance QuickCheck.Arbitrary Constraint where
  arbitrary = Witch.from <$> genVersionRange

genVersionRange :: QuickCheck.Gen Cabal.VersionRange
genVersionRange =
  QuickCheck.oneof
    [ Cabal.thisVersion <$> genVersion,
      Cabal.laterVersion <$> genVersion,
      Cabal.earlierVersion <$> genVersion,
      Cabal.orLaterVersion <$> genVersion,
      Cabal.orEarlierVersion <$> genVersion,
      Cabal.majorBoundVersion <$> genVersion,
      Cabal.unionVersionRanges <$> genVersionRange <*> genVersionRange,
      Cabal.intersectVersionRanges <$> genVersionRange <*> genVersionRange
    ]

genVersion :: QuickCheck.Gen Cabal.Version
genVersion = Witch.from @VersionNumber.VersionNumber <$> QuickCheck.arbitrary

any :: Constraint
any = Witch.from @Cabal.VersionRange Cabal.anyVersion

includes :: VersionNumber.VersionNumber -> Constraint -> Bool
includes v = Cabal.withinRange (Witch.into @Cabal.Version v) . Witch.into @Cabal.VersionRange
