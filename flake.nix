{
  description = "Reflex App";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    flake-compat.url = github:edolstra/flake-compat;
    flake-compat.flake = false;
    flake-utils.url = "github:numtide/flake-utils";
    reflex-platform.url = "github:reflex-frp/reflex-platform/develop";
    reflex-platform.flake = false;
  };

  outputs = { self, nixpkgs, flake-compat, flake-utils, reflex-platform }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        reflexPlatform = import reflex-platform { inherit system; __useNewerCompiler = true; };
        pkgs = nixpkgs.legacyPackages."${system}".appendOverlays [
          self.overlay."${system}"
        ];
      in
      {
        overlay = final: prev: {
          ghc = reflexPlatform.ghc.override {
            overrides = hfinal: hprev: {
              backend = hprev.callCabal2nix "backend" ./backend { };
            };
          };
          ghcjs = reflexPlatform.ghcjs;
        };

        devShell = pkgs.ghc.shellFor {
          packages = p: [ p.backend ];
          buildInputs = [
            pkgs.ghcjs.ghc
            pkgs.ghc.ghc
          ];
        };
      });
}
