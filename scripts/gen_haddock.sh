#!/bin/sh

mkdir -p ./docs
rm -rf ./docs/*
cabal haddock --haddock-hyperlink-source
cp -r dist-newstyle/build/x86_64-linux/ghc-8.10.7/navi-0.1.0.0/doc/html/navi/* ./docs
