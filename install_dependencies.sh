#!/bin/sh
cabal install parsers
cabal install parsec
cabal install text
cabal install HUnit
cabal configure -v --enable-tests
cabal build
