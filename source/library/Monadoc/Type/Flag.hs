module Monadoc.Type.Flag where

data Flag
  = Database String
  | Help
  | Host String
  | Port String
  | Version
  deriving (Eq, Show)
