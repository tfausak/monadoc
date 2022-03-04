module Monadoc.Exception.InvalidOption where

import qualified Control.Monad.Catch as Exception

newtype InvalidOption
  = InvalidOption String
  deriving (Eq, Show)

instance Exception.Exception InvalidOption
