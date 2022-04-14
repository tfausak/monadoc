#! /usr/bin/env sh
set -o errexit -o xtrace

cabal update

exec fswatch \
  --event Created \
  --event Removed \
  --event Renamed \
  --event Updated \
  --one-per-batch \
  --recursive \
  cabal.project monadoc.cabal source \
  | xargs -I % cabal test
