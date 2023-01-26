.PHONY: build clean repl watch ;\
	test unit integration functional ;\
	cic ci formatc format lint lintc ;\
	haddock haddockc hackage

# core

ARGS = ""

build:
	if [ -z "$(ARGS)" ]; then \
		cabal build; \
	else \
		cabal build $(ARGS); \
	fi

clean:
	cabal clean

test:
	if [ -z "$(ARGS)" ]; then \
		cabal test; \
	else \
		cabal test $(ARGS); \
	fi

unit:
	cabal test unit

integration:
	cabal test integration

repl:
	if [ -z "$(ARGS)" ]; then \
		cabal repl navi; \
	else \
		cabal repl $(ARGS); \
	fi

watch:
	if [ -z "$(ARGS)" ]; then \
		ghcid --command "cabal repl navi"; \
	else \
		ghcid --command "cabal repl $(ARGS)"; \
	fi

# ci

cic: formatc lintc haddockc

ci: lint format haddockc

# formatting

formatc:
	nix run github:tbidne/nix-hs-tools/0.7#nixpkgs-fmt -- --check ;\
	nix run github:tbidne/nix-hs-tools/0.7#cabal-fmt -- --check ;\
	nix run github:tbidne/nix-hs-tools/0.7#ormolu -- --mode check

format:
	nix run github:tbidne/nix-hs-tools/0.7#nixpkgs-fmt ;\
	nix run github:tbidne/nix-hs-tools/0.7#cabal-fmt -- --inplace ;\
	nix run github:tbidne/nix-hs-tools/0.7#ormolu -- --mode inplace

# linting

lint:
	nix run github:tbidne/nix-hs-tools/0.7#hlint -- --refact

lintc:
	nix run github:tbidne/nix-hs-tools/0.7#hlint

haddock:
	cabal haddock --haddock-hyperlink-source --haddock-quickjump ;\
	mkdir -p docs/ ;\
	find docs/ -type f | xargs -I % sh -c "rm -r %" ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.2.5/navi-0.1/opt/doc/html/navi/* docs/

haddockc:
	nix run github:tbidne/nix-hs-tools/0.7#haddock-cov -- \
	. \
	-m Navi.Config.Types 55 \
	-m Navi.Data.NaviNote 80 \
	-m Navi.Data.NaviQueue 80 \
	-m Navi.Event.Toml 60 \
	-m Navi.Event.Types 55 \
	-m Navi.Prelude 85
