#! /usr/bin/env sh
set -o errexit -o xtrace

cabal update

if ! test -f cabal.project.local
then
  cabal configure --disable-optimization --enable-tests --flags parallel --jobs --test-show-details direct
fi
