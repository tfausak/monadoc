#! /usr/bin/env sh
set -o errexit

if ! command -v hlint > /dev/null
then
  cabal install hlint
fi

exec hlint "$@"
