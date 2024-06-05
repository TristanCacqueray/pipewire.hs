ci:
  cabal-gild --io ./pw-controller.cabal --io ./pipewire/pipewire.cabal
  fourmolu -i *.hs ./pipewire
  cabal build --ghc-options="-Wall -Werror" --flags=examples -O0 all
  cabal repl --with-ghc=doctest exe:pw-controller

dev target:
  watchexec cabal build -O0 --flag=examples exe:{{target}}

doc:
  cabal haddock lib:pipewire
