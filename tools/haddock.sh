set -e

export LANG="C.UTF-8"

cabal update
cabal haddock --haddock-hyperlink-source --haddock-quickjump

mkdir -p docs/

# shellcheck disable=SC2038
find docs/ -type f | xargs -I % sh -c "rm -r %"

cp -r dist-newstyle/build/x86_64-linux/ghc-9.4.4/navi-0.1/doc/html/navi/* docs/