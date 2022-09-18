#! /usr/bin/env sh
set -o errexit -o xtrace

ghc --version
cabal --version

if ! test -d ~/.cabal/packages
then
  cabal --ignore-project update
fi

if ! test -f cabal.project.local
then
  cabal configure --disable-optimization --enable-tests --flags parallel --jobs --test-show-details direct
fi

cabal update
