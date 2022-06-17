#! /usr/bin/env sh
set -o errexit

if ! command -v ormolu > /dev/null
then
  cabal install ormolu
fi

exec ormolu "$@"
