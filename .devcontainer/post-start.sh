#! /usr/bin/env sh
set -o errexit -o xtrace

git config --global --add safe.directory "$PWD"
cabal update
