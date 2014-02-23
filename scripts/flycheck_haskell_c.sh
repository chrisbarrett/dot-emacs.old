#!/usr/bin/env bash
#
# Script for Haskell-C flycheck checker. Compiles the given `hsc` file to an
# `hs` file, then compiles with GHC.
#
# ARGUMENTS:
#
# The first argument is the path to the `hsc` file. The remaining arguments are
# passed to ghc.

FILE=$(mktemp -t flycheck).hs
touch "$FILE"

hsc2hs "$1" -o "$FILE"

# Shift arguments to remove $1 from argv.
shift

ghc -Wall -fno-code "$@" "$FILE"

RESULT=$?
rm -rf "$DIR"
exit $RESULT
