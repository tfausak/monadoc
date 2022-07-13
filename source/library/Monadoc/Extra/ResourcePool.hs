{-# LANGUAGE FlexibleContexts #-}

module Monadoc.Extra.ResourcePool where

import qualified Control.Monad.Trans.Control as Control
import qualified Data.Pool as Pool

withResourceLifted ::
  Control.MonadBaseControl IO m =>
  Pool.Pool a ->
  (a -> m b) ->
  m b
withResourceLifted = Control.liftBaseOp . Pool.withResource
