module Monadoc.Type.Severity where

import qualified Data.Char as Char
import qualified Witch

data Severity
  = Debug
  | Info
  | Warn
  | Error
  deriving (Eq, Ord, Show)

instance Witch.From Severity String where
  from = fmap Char.toLower . show
