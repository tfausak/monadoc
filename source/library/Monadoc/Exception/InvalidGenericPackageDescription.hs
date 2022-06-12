module Monadoc.Exception.InvalidGenericPackageDescription where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString

newtype InvalidGenericPackageDescription
  = InvalidGenericPackageDescription ByteString.ByteString
  deriving (Eq, Show)

instance Exception.Exception InvalidGenericPackageDescription
