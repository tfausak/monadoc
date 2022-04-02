module Monadoc.Type.VersionNumber where

import qualified Data.Version as Version
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.Version as Cabal
import qualified Lucid
import qualified Witch

newtype VersionNumber
  = VersionNumber Cabal.Version
  deriving (Eq, Ord, Show)

instance Witch.From Cabal.Version VersionNumber

instance Witch.From VersionNumber Cabal.Version

instance Witch.TryFrom String VersionNumber where
  tryFrom = Witch.maybeTryFrom $ fmap (Witch.from @Cabal.Version) . Cabal.simpleParsec

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
  toHtmlRaw = Lucid.toHtml

instance Witch.From Version.Version VersionNumber where
  from = Witch.from @Cabal.Version . Cabal.mkVersion'

instance Witch.From VersionNumber Version.Version where
  from = Version.makeVersion . Cabal.versionNumbers . Witch.into @Cabal.Version
