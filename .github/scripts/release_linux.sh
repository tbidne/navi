#!/usr/bin/env bash

set -e

arch=$(uname -m)

dir=$1

mkdir -p bin

suffix="$NAVI_VERS-$arch-linux"

docker build \
  -t navi_build:latest \
  -f "docker/$dir/Dockerfile" \
  -o docker_out \
  --build-arg CABAL_VERS=$CABAL_VERS \
  --build-arg CABAL_PROJ=$CABAL_PROJ \
  --build-arg GHC_VERS=$GHC_VERS \
  .

cp docker_out/navi bin/

echo "*** Testing exe ***"
./bin/navi --help

echo "*** Printing version ***"
./bin/navi --version

echo "*** Computing sha256 ***"
sha256sum ./bin/navi > ./bin/navi.sha256
cat ./bin/navi.sha256

# -j needed to keep structure flat
zip "navi_$suffix.zip" -j ./bin/*
