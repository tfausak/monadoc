module Monadoc.Exception.InvalidSize where

import qualified Control.Exception.Base as Exception

data InvalidSize
  = InvalidSize Int Int
  deriving (Eq, Show)

instance Exception.Exception InvalidSize
