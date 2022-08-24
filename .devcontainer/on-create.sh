#! /usr/bin/env sh
set -o errexit -o xtrace

ghc --version
cabal --version

test -d ~/.cabal/packages ||
  cabal --ignore-project update

test -f cabal.project.local ||
  cabal configure --enable-tests --flags parallel --jobs --test-show-details direct

cabal update
