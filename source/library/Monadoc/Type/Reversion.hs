module Monadoc.Type.Reversion where

import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import qualified Distribution.Compat.CharParsing as Cabal
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Types.Version as Cabal
import qualified Lucid
import qualified Monadoc.Extra.Either as Either
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.VersionNumber as VersionNumber
import qualified Witch

data Reversion = Reversion
  { revision :: Maybe Revision.Revision,
    version :: VersionNumber.VersionNumber
  }
  deriving (Eq, Show)

instance Witch.From Reversion String where
  from reversion =
    Witch.into @String (version reversion)
      <> maybe "" (mappend "+" . Witch.into @String) (revision reversion)

instance Witch.From Reversion Text.Text where
  from = Witch.via @String

instance Lucid.ToHtml Reversion where
  toHtml = Lucid.toHtml . Witch.into @String
  toHtmlRaw = Lucid.toHtmlRaw . Witch.into @String

instance Witch.TryFrom String Reversion where
  tryFrom = Witch.maybeTryFrom $ Either.hush . Cabal.explicitEitherParsec parseReversion

instance Witch.TryFrom Text.Text Reversion where
  tryFrom = Witch.eitherTryFrom $ Witch.tryFrom . Witch.into @String

parseReversion :: Cabal.CabalParsing m => m Reversion
parseReversion = do
  v <- parseVersion
  r <- Cabal.optional parseRevision
  Cabal.eof
  pure Reversion {revision = r, version = v}

parseVersion :: Cabal.CabalParsing m => m VersionNumber.VersionNumber
parseVersion = do
  -- This can't use 'Cabal.parsec' because the instance for 'Cabal.Version'
  -- parses and discards tags. That means @"1-2"@ would successfully parse when
  -- it shouldn't.
  fmap (Witch.from @Cabal.Version . Cabal.mkVersion . Foldable.toList)
    . Cabal.sepByNonEmpty Cabal.versionDigitParser
    $ Cabal.char '.'

parseRevision :: Cabal.CabalParsing m => m Revision.Revision
parseRevision = do
  Monad.void $ Cabal.char '+'
  Witch.from @Word <$> Cabal.integral
