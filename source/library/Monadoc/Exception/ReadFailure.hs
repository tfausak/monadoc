module Monadoc.Exception.ReadFailure where

import qualified Control.Monad.Catch as Exception

data ReadFailure
  = ReadFailure String String
  deriving (Eq, Show)

instance Exception.Exception ReadFailure
