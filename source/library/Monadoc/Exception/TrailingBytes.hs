module Monadoc.Exception.TrailingBytes where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString

newtype TrailingBytes
  = TrailingBytes ByteString.ByteString
  deriving (Eq, Show)

instance Exception.Exception TrailingBytes
