module Monadoc.Exception.UnknownOption where

import qualified Control.Monad.Catch as Exception

newtype UnknownOption
  = UnknownOption String
  deriving (Eq, Show)

instance Exception.Exception UnknownOption
