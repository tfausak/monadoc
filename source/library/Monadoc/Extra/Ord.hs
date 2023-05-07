module Monadoc.Extra.Ord where

clamp :: (Ord a) => a -> a -> a -> a
clamp lo hi = max lo . min hi
