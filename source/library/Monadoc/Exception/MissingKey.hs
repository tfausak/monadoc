module Monadoc.Exception.MissingKey where

import qualified Control.Monad.Catch as Exception

data MissingKey
  = MissingKey
  deriving (Eq, Show)

instance Exception.Exception MissingKey
