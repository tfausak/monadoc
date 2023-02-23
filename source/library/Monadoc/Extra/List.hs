module Monadoc.Extra.List where

ensureSuffix :: (Eq a) => a -> [a] -> [a]
ensureSuffix z xs = case xs of
  [] -> [z]
  [x] -> if x == z then xs else [x, z]
  x : ys -> x : ensureSuffix z ys
