module Monadoc.Type.Flag where

data Flag
  = Help
  | Host String
  | Port String
  | Version
  deriving (Eq, Show)
