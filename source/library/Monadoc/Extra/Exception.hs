{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Extra.Exception where

import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Exception (AsyncException, SomeAsyncException)
import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Exception.Traced as Traced

isAsync :: Exception.SomeException -> Bool
isAsync e =
  isType @Async.AsyncCancelled e
    || isType @Exception.AsyncException e
    || isType @Exception.SomeAsyncException e

isNotType :: forall e. Exception.Exception e => Exception.SomeException -> Bool
isNotType = not . isType @e

isSync :: Exception.SomeException -> Bool
isSync = not . isAsync

isType :: forall e. Exception.Exception e => Exception.SomeException -> Bool
isType x
  | Just (Traced.Traced y _) <- Exception.fromException x = isType @e y
  | Just _ <- Exception.fromException @e x = True
  | otherwise = False
