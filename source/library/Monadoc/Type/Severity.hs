module Monadoc.Type.Severity where

data Severity
  = Debug
  | Info
  | Warn
  | Error
  deriving (Eq, Ord, Show)
