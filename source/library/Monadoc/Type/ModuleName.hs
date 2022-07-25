module Monadoc.Type.ModuleName where

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Lucid as Html
import qualified Monadoc.Extra.Cabal as Cabal
import qualified Monadoc.Extra.Either as Either
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

newtype ModuleName
  = ModuleName Cabal.ModuleName
  deriving (Eq, Ord, Show)

instance Witch.From Cabal.ModuleName ModuleName

instance Witch.From ModuleName Cabal.ModuleName

instance Witch.TryFrom String ModuleName where
  tryFrom = Witch.eitherTryFrom $ fmap (Witch.from @Cabal.ModuleName) . Cabal.tryParsec

instance Witch.TryFrom Text.Text ModuleName where
  tryFrom = Witch.eitherTryFrom $ Witch.tryFrom . Witch.into @String

instance Witch.From ModuleName String where
  from = Cabal.prettyShow . Witch.into @Cabal.ModuleName

instance Witch.From ModuleName Text.Text where
  from = Witch.via @String

instance Sql.FromField ModuleName where
  fromField field = do
    string <- Sql.fromField @String field
    either (Sql.returnError Sql.ConversionFailed field . show) pure $
      Witch.tryFrom string

instance Sql.ToField ModuleName where
  toField = Sql.toField . Witch.into @String

instance Html.ToHtml ModuleName where
  toHtml = Html.toHtml . Witch.into @String
  toHtmlRaw = Html.toHtmlRaw . Witch.into @String

instance QuickCheck.Arbitrary ModuleName where
  arbitrary = QuickCheck.suchThatMap genString $ Either.hush . Witch.tryFrom

genString :: QuickCheck.Gen String
genString = List.intercalate "." <$> QuickCheck.listOf1 genPart

genPart :: QuickCheck.Gen String
genPart = (:) <$> QuickCheck.elements ['A' .. 'Z'] <*> QuickCheck.listOf genAlphaNum

genAlphaNum :: QuickCheck.Gen Char
genAlphaNum =
  QuickCheck.elements $
    mconcat
      [ ['A' .. 'Z'],
        ['a' .. 'z'],
        ['0' .. '9']
      ]
