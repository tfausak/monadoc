module Monadoc.Type.BuildType where

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.BuildType as Cabal
import qualified Lucid as Html
import qualified Monadoc.Extra.Cabal as Cabal
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

newtype BuildType
  = BuildType Cabal.BuildType
  deriving (Eq, Show)

instance Witch.From Cabal.BuildType BuildType

instance Witch.From BuildType Cabal.BuildType

instance Witch.TryFrom String BuildType where
  tryFrom =
    Witch.eitherTryFrom $
      fmap (Witch.from @Cabal.BuildType)
        . Cabal.tryParsec

instance Witch.From BuildType String where
  from = Cabal.prettyShow . Witch.into @Cabal.BuildType

instance Sql.FromField BuildType where
  fromField field = do
    string <- Sql.fromField @String field
    either (Sql.returnError Sql.ConversionFailed field . show) pure $
      Witch.tryFrom string

instance Sql.ToField BuildType where
  toField = Sql.toField . Witch.into @String

instance Html.ToHtml BuildType where
  toHtml = Html.toHtml . Witch.into @String
  toHtmlRaw = Html.toHtmlRaw . Witch.into @String

instance QuickCheck.Arbitrary BuildType where
  arbitrary = Witch.from <$> genBuildType

genBuildType :: QuickCheck.Gen Cabal.BuildType
genBuildType = QuickCheck.elements Cabal.knownBuildTypes

simple :: BuildType
simple = Witch.from Cabal.Simple
