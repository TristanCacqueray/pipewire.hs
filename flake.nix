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
        "pipewire" = hpPrev.callCabal2nixWithOptions "pipewire" ./pipewire
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
        packages = p: [ p.${name} p.pipewire ];
        buildInputs = ciTools ++ devTools;
      };
    };
}
