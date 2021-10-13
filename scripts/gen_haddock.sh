#!/bin/sh

mkdir -p ./docs
rm -rf ./docs/*
cabal haddock --haddock-hyperlink-source
cp -r dist-newstyle/build/x86_64-linux/ghc-8.10.4/byte-types-0.1.0.0/doc/html/byte-types/* ./docs
