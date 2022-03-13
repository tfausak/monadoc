module Monadoc.Exception.MissingSize where

import qualified Control.Exception.Base as Exception
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Network.HTTP.Client as Client

newtype MissingSize
  = MissingSize (Client.Response LazyByteString.ByteString)
  deriving (Show)

instance Exception.Exception MissingSize
