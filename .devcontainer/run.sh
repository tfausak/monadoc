#! /usr/bin/env sh
set -o errexit -o xtrace

if ! command -v "$1" 1>&2
then
  cabal install "$1"
fi

exec "$@"
