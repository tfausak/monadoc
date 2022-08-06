#! /usr/bin/env sh
set -o errexit -o xtrace

ghc --version
cabal --version

test -f cabal.project.local ||
  cabal configure --enable-tests --flags parallel --jobs --test-show-details direct

docker/devcontainer/cabal-shim.sh hlint --version
docker/devcontainer/cabal-shim.sh ormolu --version
