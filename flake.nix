{
  description = "pw-controller";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs = { self, nixpkgs }:
    let
      name = "pw-controller";
      pkgs = import nixpkgs {
        localSystem = "x86_64-linux";
        config.allowUnfree = true;
      };

      # The haskell package set override
      haskellExtend = hpFinal: hpPrev: {
        ${name} = hpPrev.callCabal2nix name ./. { };
      };
      hsPkgs = pkgs.haskellPackages.extend haskellExtend;

      ciTools = [
        pkgs.cabal-install
        pkgs.haskellPackages.fourmolu
        pkgs.hlint
      ];
      devTools = [
        pkgs.haskell-language-server
        pkgs.ghcid
        pkgs.haskellPackages.cabal-fmt
        pkgs.just
        pkgs.haskellPackages.doctest
      ];
    in {
      devShell.x86_64-linux = hsPkgs.shellFor {
        packages = p: [
          p.${name}
        ];
        buildInputs = ciTools ++ devTools;
      };
    };
}
