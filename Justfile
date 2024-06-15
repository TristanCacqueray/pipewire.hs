ci: fmt build test-controller

fmt:
  cabal-gild --io ./pw-controller/pw-controller.cabal --io ./pipewire/pipewire.cabal
  fourmolu -i ./pw-controller ./pipewire

build:
  cabal build --ghc-options="-Wall -Werror" --flags=examples -O0 all

test-controller:
  cabal repl --with-ghc=doctest exe:pw-controller

dev target:
  watchexec cabal build -O0 --flag=examples exe:{{target}}

doc:
  cabal haddock lib:pipewire
