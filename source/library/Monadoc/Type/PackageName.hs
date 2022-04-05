module Monadoc.Type.PackageName where

import qualified Data.Text as Text
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.Package as Cabal
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Lucid
import qualified Witch

newtype PackageName
  = PackageName Cabal.PackageName
  deriving (Eq, Ord, Show)

instance Witch.From Cabal.PackageName PackageName

instance Witch.From PackageName Cabal.PackageName

instance Witch.TryFrom String PackageName where
  tryFrom = Witch.maybeTryFrom $ fmap (Witch.from @Cabal.PackageName) . Cabal.simpleParsec

instance Witch.From PackageName String where
  from = Cabal.prettyShow . Witch.into @Cabal.PackageName

instance Witch.TryFrom Text.Text PackageName where
  tryFrom = Witch.eitherTryFrom $ Witch.tryInto @PackageName . Witch.into @String

instance Witch.From PackageName Text.Text where
  from = Witch.via @String

instance Sql.FromField PackageName where
  fromField field = do
    string <- Sql.fromField @String field
    either (Sql.returnError Sql.ConversionFailed field . show) pure $
      Witch.tryFrom string

instance Sql.ToField PackageName where
  toField = Sql.toField . Witch.into @String

instance Lucid.ToHtml PackageName where
  toHtml = Lucid.toHtml . Witch.into @String
  toHtmlRaw = Lucid.toHtmlRaw . Witch.into @String
