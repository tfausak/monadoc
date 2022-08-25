#! /usr/bin/env sh
set -o errexit -o xtrace

ghc --version
cabal --version

test -d ~/.cabal/packages ||
  cabal --ignore-project update

.devcontainer/cabal-shim.sh hlint --version
.devcontainer/cabal-shim.sh ormolu --version

test -f cabal.project.local ||
  cabal configure --enable-tests --flags parallel --jobs --test-show-details direct

cabal update