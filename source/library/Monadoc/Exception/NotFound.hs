module Monadoc.Exception.NotFound where

import qualified Control.Monad.Catch as Exception

newtype NotFound
  = NotFound String
  deriving (Eq, Show)

instance Exception.Exception NotFound
