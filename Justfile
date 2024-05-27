ci:
  cabal build --flags=examples -O0 all
  cabal repl --with-ghc=doctest exe:pw-controller
  fourmolu -i *.hs ./pipewire
  cabal-fmt -i ./pw-controller.cabal ./pipewire/pipewire.cabal

dev:
  watchexec cabal build -O0 --flag=examples exe:tutorial3
