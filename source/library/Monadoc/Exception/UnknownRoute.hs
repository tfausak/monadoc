module Monadoc.Exception.UnknownRoute where

import qualified Control.Monad.Catch as Exception
import qualified Data.Text as Text

newtype UnknownRoute
  = UnknownRoute [Text.Text]
  deriving (Eq, Show)

instance Exception.Exception UnknownRoute
