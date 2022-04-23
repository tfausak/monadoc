module Monadoc.Exception.MissingRowid where

import qualified Control.Monad.Catch as Exception

data MissingRowid
  = MissingRowid
  deriving (Eq, Show)

instance Exception.Exception MissingRowid
