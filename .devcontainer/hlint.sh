#! /usr/bin/env sh
set -o errexit -o xtrace

if ! command -v hlint 1>&2
then
  cabal install hlint
fi

exec hlint "$@"
