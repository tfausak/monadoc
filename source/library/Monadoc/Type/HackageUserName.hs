module Monadoc.Type.HackageUserName where

import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Lucid as Html
import qualified Monadoc.Extra.Either as Either
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

newtype HackageUserName
  = HackageUserName String
  deriving (Eq, Show)

instance Witch.TryFrom String HackageUserName where
  tryFrom = Witch.maybeTryFrom $ \string -> do
    -- Hackage user names must be non-empty and contain ASCII alphanumerics.
    -- https://github.com/haskell/hackage-server/blob/c3a876a/src/Distribution/Server/Users/Types.hs#L89
    Monad.guard . not $ null string
    let p c = Char.isAscii c && Char.isAlphaNum c || c == '_'
    Monad.guard $ all p string
    pure $ HackageUserName string

instance Witch.From HackageUserName String

instance Sql.FromField HackageUserName where
  fromField field = do
    string <- Sql.fromField @String field
    either (Sql.returnError Sql.ConversionFailed field . show) pure $
      Witch.tryFrom string

instance Sql.ToField HackageUserName where
  toField = Sql.toField . Witch.into @String

instance Html.ToHtml HackageUserName where
  toHtml = Html.toHtml . Witch.into @String
  toHtmlRaw = Html.toHtmlRaw . Witch.into @String

instance QuickCheck.Arbitrary HackageUserName where
  arbitrary = QuickCheck.suchThatMap genString $ Either.hush . Witch.tryFrom

instance Witch.TryFrom Text.Text HackageUserName where
  tryFrom = Witch.eitherTryFrom $ Witch.tryFrom . Witch.into @String

instance Witch.From HackageUserName Text.Text where
  from = Witch.via @String

genString :: QuickCheck.Gen String
genString = QuickCheck.listOf1 genChar

genChar :: QuickCheck.Gen Char
genChar =
  QuickCheck.elements $
    mconcat
      [['A' .. 'Z'], ['a' .. 'z'], ['0' .. '9'], "_"]
