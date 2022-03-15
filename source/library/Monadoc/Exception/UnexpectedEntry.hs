module Monadoc.Exception.UnexpectedEntry where

import qualified Codec.Archive.Tar as Tar
import qualified Control.Monad.Catch as Exception

newtype UnexpectedEntry
  = UnexpectedEntry Tar.Entry
  deriving (Eq, Show)

instance Exception.Exception UnexpectedEntry
