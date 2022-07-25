module Monadoc.Type.Severity where

import qualified Witch

data Severity
  = Debug
  | Info
  | Warn
  | Error
  deriving (Eq, Ord, Show)

instance Witch.From Severity String where
  from severity = case severity of
    Debug -> "debug"
    Info -> "info"
    Warn -> "warn"
    Error -> "error"

instance Witch.TryFrom String Severity where
  tryFrom = Witch.maybeTryFrom $ \string -> case string of
    "debug" -> Just Debug
    "info" -> Just Info
    "warn" -> Just Warn
    "error" -> Just Error
    _ -> Nothing
