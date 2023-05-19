module Monadoc.Type.RequestId where

import qualified Control.Monad.IO.Class as IO
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Word as Word
import qualified Formatting as F
import qualified System.Random as Random
import qualified Witch

newtype RequestId
  = RequestId Word.Word32
  deriving (Eq, Show)
  deriving (Random.Random) via Word.Word32

instance Witch.From Word.Word32 RequestId

instance Witch.From RequestId Word.Word32

instance Witch.From RequestId Text.Text where
  from = F.sformat format

instance Witch.From RequestId ByteString.ByteString where
  from = Witch.via @(Witch.UTF_8 ByteString.ByteString) . Witch.into @Text.Text

instance Aeson.ToJSON RequestId where
  toEncoding = Aeson.toEncoding . Witch.into @Text.Text
  toJSON = Aeson.toJSON . Witch.into @Text.Text

format :: F.Format r (RequestId -> r)
format = F.later $ F.bprint (F.hexPrefix 8) . Witch.into @Word.Word32

random :: (IO.MonadIO m) => m RequestId
random = Random.randomIO

zero :: RequestId
zero = Witch.from @Word.Word32 0
