{
  description = "pipewire.hs";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs = { self, nixpkgs }:
    let
      inherit (nixpkgs) lib;

      system = "x86_64-linux";
      pkgs = import nixpkgs {
        localSystem = system;
        config.allowUnfree = true;
        overlays = [ self.overlays.default ];
      };

      # The haskell package set override
      haskellExtend = hpFinal: hpPrev: {
        pw-controller = hpPrev.callCabal2nix "pw-controller" ./pw-controller { };
        pipewire = hpPrev.callCabal2nixWithOptions "pipewire" ./pipewire
          "--flag=examples" { libpipewire = pkgs.pipewire; };
      };
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
      legacyPackages.${system} = pkgs;

      overlays = {
        default = lib.composeManyExtensions [ self.overlays.pw-mon self.overlays.hs ];

        # Remove this overlay once the pw-mon change is available in a release.
        # https://gitlab.freedesktop.org/pipewire/pipewire/-/merge_requests/1998
        pw-mon = final: prev: {
          pipewire = prev.pipewire.overrideAttrs (oa: {
            patches = oa.patches or [ ] ++ [
              (final.fetchpatch {
                name = "pipewire-mr-1998.patch";
                url = "https://gitlab.freedesktop.org/pipewire/pipewire/-/commit/47a71325d6289a89a2d0486c798fb2b58ebeb49b.patch";
                hash = "sha256-2IrtM3Mf/I0DoAcbf16SqOqhaXvlnKom6dx7dloNKVw=";
              })
            ];
          });
        };

        hs = final: prev: {
          haskell = prev.haskell // {
            packageOverrides = lib.composeExtensions
              prev.haskell.packageOverrides
              haskellExtend;
          };
        };
      };

      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        packages = p: [ p.pw-controller p.pipewire ];
        buildInputs = ciTools ++ devTools;
      };
    };
}
