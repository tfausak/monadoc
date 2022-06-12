{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Extra.Read where

import qualified Monadoc.Extra.Maybe as Maybe
import qualified Text.Read as Read
import qualified Witch

tryRead ::
  (Read t, Witch.From s String) =>
  s ->
  Either (Witch.TryFromException s t) t
tryRead s =
  Maybe.note (Witch.TryFromException s Nothing)
    . Read.readMaybe
    $ Witch.into @String s
