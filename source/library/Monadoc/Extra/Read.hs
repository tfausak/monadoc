module Monadoc.Extra.Read where

import qualified Monadoc.Extra.Either as Either
import qualified Text.Read as Read
import qualified Witch

tryRead ::
  forall t s.
  (Witch.From s String, Read t) =>
  s ->
  Either (Witch.TryFromException s t) t
tryRead s =
  Either.note (Witch.TryFromException s Nothing)
    . Read.readMaybe
    $ Witch.into @String s
