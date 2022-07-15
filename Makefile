# core

ARGS = ""

.PHONY: build
build:
	if [ -z "$(ARGS)" ]; then \
		cabal build; \
	else \
		cabal build $(ARGS); \
	fi

.PHONY: clean
clean:
	cabal clean

.PHONY: test
test:
	if [ -z "$(ARGS)" ]; then \
		cabal test; \
	else \
		cabal test $(ARGS); \
	fi

.PHONY: unit
unit:
	cabal test unit

.PHONY: integration
integration:
	cabal test integration

.PHONY: repl
repl:
	if [ -z "$(ARGS)" ]; then \
		cabal repl; \
	else \
		cabal repl $(ARGS); \
	fi

.PHONY: watch
watch:
	ghcid --command "cabal repl $(ARGS)"

# ci

.PHONY: cic
cic: formatc lintc haddockc

.PHONY: ci
ci: lint format

# formatting

.PHONY: formatc
formatc: cabalfmtc hsformatc nixpkgsfmtc

.PHONY: format
format: cabalfmt hsformat nixpkgsfmt

.PHONY: hsformat
hsformat:
	nix run github:tbidne/nix-hs-tools/0.6#ormolu -- --mode inplace

.PHONY: hsformatc
hsformatc:
	nix run github:tbidne/nix-hs-tools/0.6#ormolu -- --mode check

.PHONY: cabalfmt
cabalfmt:
	nix run github:tbidne/nix-hs-tools/0.6#cabal-fmt -- --inplace

.PHONY: cabalfmtc
cabalfmtc:
	nix run github:tbidne/nix-hs-tools/0.6#cabal-fmt -- --check

.PHONY: nixpkgsfmt
nixpkgsfmt:
	nix run github:tbidne/nix-hs-tools/0.6#nixpkgs-fmt

.PHONY: nixpkgsfmtc
nixpkgsfmtc:
	nix run github:tbidne/nix-hs-tools/0.6#nixpkgs-fmt -- --check

# linting

.PHONY: lint
lint:
	nix run github:tbidne/nix-hs-tools/0.6#hlint -- --refact

.PHONY: lintc
lintc:
	nix run github:tbidne/nix-hs-tools/0.6#hlint

.PHONY: haddock
haddock:
	cabal haddock --haddock-hyperlink-source --haddock-quickjump ;\
	mkdir -p docs/ ;\
	find docs/ -type f | xargs -I % sh -c "rm -r %" ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.2.3/navi-0.1/opt/doc/html/navi/* docs/

.PHONY: haddockc
haddockc:
	nix run github:tbidne/nix-hs-tools/0.6#haddock-cov -- . -x Navi.Prelude
