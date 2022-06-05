{-# LANGUAGE FlexibleContexts #-}

module Monadoc.Extra.ResourcePool where

import qualified Control.Monad.Trans.Control as Control
import qualified Data.Pool as Pool

liftResource :: Control.MonadBaseControl IO m => Pool.Pool a -> (a -> m b) -> m b
liftResource = Control.liftBaseOp . Pool.withResource
