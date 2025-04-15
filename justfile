default:
  just --list

build:
  cabal build

build-site:
  cabal run site build

watch-site:
  cabal run site watch

watch:
  fd ".*\.hs" . | entr -r -c just build watch-site
