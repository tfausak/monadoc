module Monadoc.Exception.InvalidDsn where

import qualified Control.Monad.Catch as Exception

newtype InvalidDsn
  = InvalidDsn String
  deriving (Eq, Show)

instance Exception.Exception InvalidDsn
