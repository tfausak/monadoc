module Monadoc.Type.VersionNumber where

import qualified Data.Text as Text
import qualified Data.Version as Version
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.Version as Cabal
import qualified Lucid as Html
import qualified Monadoc.Extra.Cabal as Cabal
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

newtype VersionNumber
  = VersionNumber Cabal.Version
  deriving (Eq, Ord, Show)

instance Witch.From Cabal.Version VersionNumber

instance Witch.From VersionNumber Cabal.Version

instance Witch.TryFrom String VersionNumber where
  tryFrom =
    Witch.eitherTryFrom $
      fmap (Witch.from @Cabal.Version)
        . Cabal.tryParsec

instance Witch.From VersionNumber String where
  from = Cabal.prettyShow . Witch.into @Cabal.Version

instance Sql.FromField VersionNumber where
  fromField field = do
    string <- Sql.fromField @String field
    either (Sql.returnError Sql.ConversionFailed field . show) pure $
      Witch.tryFrom string

instance Sql.ToField VersionNumber where
  toField = Sql.toField . Witch.into @String

instance Html.ToHtml VersionNumber where
  toHtml = Html.toHtml . Witch.into @String
  toHtmlRaw = Html.toHtmlRaw . Witch.into @String

instance Witch.From Version.Version VersionNumber where
  from = Witch.from @Cabal.Version . Cabal.mkVersion'

instance Witch.From VersionNumber Version.Version where
  from = Version.makeVersion . Cabal.versionNumbers . Witch.into @Cabal.Version

instance QuickCheck.Arbitrary VersionNumber where
  arbitrary = Witch.from <$> genVersion

instance Witch.TryFrom Text.Text VersionNumber where
  tryFrom =
    Witch.eitherTryFrom $
      Witch.tryInto @VersionNumber
        . Witch.into @String

instance Witch.From VersionNumber Text.Text where
  from = Witch.via @String

genVersion :: QuickCheck.Gen Cabal.Version
genVersion = Cabal.mkVersion <$> QuickCheck.listOf1 (QuickCheck.chooseInt (0, 999_999_999))

zero :: VersionNumber
zero = Witch.from Cabal.version0
