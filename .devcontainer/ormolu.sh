#! /usr/bin/env sh
set -o errexit -o xtrace

if ! command -v ormolu 1>&2
then
  cabal install ormolu
fi

exec ormolu "$@"
