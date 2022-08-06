#! /usr/bin/env sh
set -o errexit -o xtrace

test -d ~/.cabal/packages ||
  cabal --ignore-project update
