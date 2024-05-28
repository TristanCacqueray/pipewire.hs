ci:
  cabal-gild --io ./pw-controller.cabal --io ./pipewire/pipewire.cabal
  fourmolu -i *.hs ./pipewire
  cabal build --flags=examples -O0 all
  cabal repl --with-ghc=doctest exe:pw-controller

dev:
  watchexec cabal build -O0 --flag=examples exe:tutorial3
