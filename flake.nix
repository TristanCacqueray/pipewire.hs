{
  description = "pipewire.hs";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        localSystem = system;
      };

      # The haskell package set override
      haskellExtend = hpFinal: hpPrev: {
        pw-controller = hpPrev.callCabal2nix "pw-controller" ./pw-controller { };
        pipewire = hpPrev.callCabal2nixWithOptions "pipewire" ./pipewire
          "--flag=examples" { libpipewire = pkgs.pipewire; };
      };
      hsPkgs = pkgs.haskellPackages.extend haskellExtend;

      ciTools = [ pkgs.cabal-install pkgs.haskellPackages.fourmolu pkgs.hlint ];
      devTools = [
        pkgs.haskell-language-server
        pkgs.ghcid
        pkgs.haskellPackages.cabal-gild
        pkgs.just
        pkgs.haskellPackages.doctest
        pkgs.pipewire
        pkgs.pkg-config
      ];
    in {
      devShell.x86_64-linux = hsPkgs.shellFor {
        packages = p: [ p.pw-controller p.pipewire ];
        buildInputs = ciTools ++ devTools;
      };
    };
}
