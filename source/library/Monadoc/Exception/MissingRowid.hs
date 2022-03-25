module Monadoc.Exception.MissingRowid where

import qualified Control.Exception.Base as Exception

data MissingRowid
  = MissingRowid
  deriving (Eq, Show)

instance Exception.Exception MissingRowid
