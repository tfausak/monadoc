module Monadoc.Exception.InvalidSize where

import qualified Control.Monad.Catch as Exception

data InvalidSize = InvalidSize
  { old :: Int,
    new :: Int
  }
  deriving (Eq, Show)

instance Exception.Exception InvalidSize
