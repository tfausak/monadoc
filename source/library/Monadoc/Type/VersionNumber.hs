module Monadoc.Type.VersionNumber where

import qualified Control.Monad as Monad
import qualified Data.Text as Text
import qualified Data.Version as Version
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.Version as Cabal
import qualified Lucid
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

instance Lucid.ToHtml VersionNumber where
  toHtml = Lucid.toHtml . Witch.into @String
  toHtmlRaw = Lucid.toHtmlRaw . Witch.into @String

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
genVersion = QuickCheck.suchThatMap QuickCheck.arbitrary toVersion

toVersion :: [Int] -> Maybe Cabal.Version
toVersion ns = do
  -- Versions must be non-empty. Numbers must be positive and less than 10 digits.
  -- https://github.com/haskell/cabal/blob/9ca9891/Cabal-syntax/src/Distribution/Types/Version.hs#L118
  Monad.guard . not $ null ns
  let p n = 0 <= n && n < (1000000000 :: Int)
  Monad.guard $ all p ns
  pure $ Cabal.mkVersion ns

zero :: VersionNumber
zero = Witch.from Cabal.version0
