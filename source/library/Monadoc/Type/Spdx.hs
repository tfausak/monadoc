module Monadoc.Type.Spdx where

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.Pretty as Cabal
import qualified Distribution.SPDX as Cabal
import qualified Monadoc.Extra.Cabal as Cabal
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

newtype Spdx
  = Spdx Cabal.License
  deriving (Eq, Show)

instance Witch.From Cabal.License Spdx

instance Witch.From Spdx Cabal.License

instance Witch.TryFrom String Spdx where
  tryFrom =
    Witch.eitherTryFrom $
      fmap (Witch.from @Cabal.License)
        . Cabal.tryParsec

instance Witch.From Spdx String where
  from = Cabal.prettyShow . Witch.into @Cabal.License

instance Sql.FromField Spdx where
  fromField field = do
    string <- Sql.fromField @String field
    either (Sql.returnError Sql.ConversionFailed field . show) pure $
      Witch.tryFrom string

instance Sql.ToField Spdx where
  toField = Sql.toField . Witch.into @String

instance QuickCheck.Arbitrary Spdx where
  arbitrary = Witch.from <$> genCabalLicense

genCabalLicense :: QuickCheck.Gen Cabal.License
genCabalLicense =
  QuickCheck.oneof
    [ pure Cabal.NONE,
      Cabal.License <$> genLicenseExpression
    ]

genLicenseExpression :: QuickCheck.Gen Cabal.LicenseExpression
genLicenseExpression = QuickCheck.sized genLicenseExpressionSized

genLicenseExpressionSized :: Int -> QuickCheck.Gen Cabal.LicenseExpression
genLicenseExpressionSized n = do
  let m = div n 2
      genELicense = Cabal.ELicense <$> genSimpleLicenseExpression <*> QuickCheck.liftArbitrary genLicenseExceptionId
  if n < 1
    then genELicense
    else
      simplify
        <$> QuickCheck.oneof
          [ genELicense,
            Cabal.EAnd <$> genLicenseExpressionSized m <*> genLicenseExpressionSized m,
            Cabal.EOr <$> genLicenseExpressionSized m <*> genLicenseExpressionSized m
          ]

-- Cabal's parser doesn't agree with its pretty printer. The association could
-- go either way, so we arbitrarily choose the one that the parser uses.
simplify :: Cabal.LicenseExpression -> Cabal.LicenseExpression
simplify le = case le of
  Cabal.EAnd (Cabal.EAnd x y) z -> simplify $ Cabal.EAnd x (Cabal.EAnd y z)
  Cabal.EOr (Cabal.EOr x y) z -> simplify $ Cabal.EOr x (Cabal.EOr y z)
  Cabal.EAnd x y -> Cabal.EAnd (simplify x) (simplify y)
  Cabal.EOr x y -> Cabal.EOr (simplify x) (simplify y)
  _ -> le

genSimpleLicenseExpression :: QuickCheck.Gen Cabal.SimpleLicenseExpression
genSimpleLicenseExpression =
  QuickCheck.oneof
    [ Cabal.ELicenseId <$> genLicenseId,
      Cabal.ELicenseIdPlus <$> genLicenseId,
      Cabal.ELicenseRef <$> genLicenseRef
    ]

-- Some license exception IDs intentionally don't round trip.
genLicenseExceptionId :: QuickCheck.Gen Cabal.LicenseExceptionId
genLicenseExceptionId = QuickCheck.elements $ Cabal.licenseExceptionIdList licenseListVersion

-- Some license IDs intentionally don't round trip.
genLicenseId :: QuickCheck.Gen Cabal.LicenseId
genLicenseId = QuickCheck.elements $ Cabal.licenseIdList licenseListVersion

licenseListVersion :: Cabal.LicenseListVersion
licenseListVersion = Cabal.LicenseListVersion_3_10

genLicenseRef :: QuickCheck.Gen Cabal.LicenseRef
genLicenseRef =
  QuickCheck.suchThatMap
    ((,) <$> QuickCheck.liftArbitrary genIdString <*> genIdString)
    (uncurry Cabal.mkLicenseRef)

-- The types allow these to be empty but the parser requires them to be
-- non-empty.
genIdString :: QuickCheck.Gen String
genIdString = QuickCheck.listOf1 genIdChar

genIdChar :: QuickCheck.Gen Char
genIdChar = QuickCheck.elements idChars

idChars :: [Char]
idChars = '-' : '.' : (['0' .. '9'] <> ['A' .. 'Z'] <> ['a' .. 'z'])

none :: Spdx
none = Witch.from Cabal.NONE
