module Monadoc.Exception.Traced where

import qualified Control.Monad.Catch as Exception
import qualified GHC.Stack as Stack

data Traced
  = Traced Exception.SomeException Stack.CallStack
  deriving (Show)

instance Exception.Exception Traced where
  displayException (Traced e s) =
    Exception.displayException e
      <> "\n"
      <> Stack.prettyCallStack s

throw :: (Stack.HasCallStack, Exception.Exception e, Exception.MonadThrow m) => e -> m a
throw = Exception.throwM . traced . Exception.toException

traced :: (Stack.HasCallStack) => Exception.SomeException -> Traced
traced = flip Traced Stack.callStack

wrap :: (Stack.HasCallStack, Exception.MonadCatch m) => m a -> m a
wrap = Exception.handleAll throw
