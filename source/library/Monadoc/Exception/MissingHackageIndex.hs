module Monadoc.Exception.MissingHackageIndex where

import qualified Control.Exception.Base as Exception

data MissingHackageIndex
  = MissingHackageIndex
  deriving (Eq, Show)

instance Exception.Exception MissingHackageIndex
