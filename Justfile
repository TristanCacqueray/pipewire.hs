ci: fmt build tests

fmt:
  cabal-gild --io ./pw-controller/pw-controller.cabal
  cabal-gild --io ./pipewire/pipewire.cabal
  fourmolu -i ./pw-controller ./pipewire

build:
  cabal build --ghc-options="-Wall -Werror" --flags=examples -O0 all

tests: doctest-controller test-controller

doctest-controller:
  cabal repl --with-ghc=doctest exe:pw-controller

test-controller:
  cabal test --enable-tests --test-show-details=direct --test-option=--accept -O0 test-pw-controller

pw-controller *args:
  cabal run -O0 exe:pw-controller -- {{args}}

example name *args:
  cabal run --flags=examples -O0 exe:{{name}} -- {{args}}

dev target:
  watchexec cabal build -O0 --flag=examples exe:{{target}}

doc:
  cabal haddock lib:pipewire

clean:
  rm -Rf dist-newstyle
