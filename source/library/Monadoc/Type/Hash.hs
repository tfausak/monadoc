module Monadoc.Type.Hash where

import qualified Control.Monad.Catch as Exception
import qualified Crypto.Hash as Crypto
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteArray.Encoding as Encoding
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Extra.Either as Either
import qualified Test.QuickCheck as QuickCheck
import qualified Witch
import qualified Witch.Utility as Witch

newtype Hash
  = Hash (Crypto.Digest Crypto.SHA256)
  deriving (Eq, Show)

instance Witch.From (Crypto.Digest Crypto.SHA256) Hash

instance Witch.From Hash (Crypto.Digest Crypto.SHA256)

instance Witch.TryFrom ByteString.ByteString Hash where
  tryFrom =
    Witch.maybeTryFrom $
      fmap Witch.from . Crypto.digestFromByteString @Crypto.SHA256

instance Witch.From Hash ByteString.ByteString where
  from = ByteArray.convert . Witch.into @(Crypto.Digest Crypto.SHA256)

instance Sql.FromField Hash where
  fromField field = do
    byteString <- Sql.fromField @ByteString.ByteString field
    either (Sql.returnError Sql.ConversionFailed field . show) pure $
      Witch.tryFrom byteString

instance Sql.ToField Hash where
  toField = Sql.toField . Witch.into @ByteString.ByteString

instance QuickCheck.Arbitrary Hash where
  arbitrary =
    QuickCheck.suchThatMap (QuickCheck.vector 32) $
      Either.hush
        . Witch.tryFrom
        . ByteString.pack

instance Witch.TryFrom Text.Text Hash where
  tryFrom text = case Encoding.convertFromBase Encoding.Base16 . Witch.into @ByteString.ByteString $ Witch.into @(Witch.UTF_8 ByteString.ByteString) text of
    Left string -> Left . Witch.TryFromException text . Just . Exception.toException $ userError string
    Right byteString -> case Witch.tryFrom @ByteString.ByteString byteString of
      Left tryFromException -> Left $ Witch.withSource text tryFromException
      Right hash -> pure hash

instance Witch.From Hash Text.Text where
  from =
    Witch.unsafeFrom @(Witch.UTF_8 ByteString.ByteString)
      . Witch.from @ByteString.ByteString
      . Encoding.convertToBase Encoding.Base16
      . Witch.into @ByteString.ByteString

new :: ByteString.ByteString -> Hash
new = Witch.from @(Crypto.Digest Crypto.SHA256) . Crypto.hash
