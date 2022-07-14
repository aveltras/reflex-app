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
        project = reflexPlatform.project
          ({ pkgs, ... }: {

            packages = { };

            shells = {
              ghc = [ ];
              ghcjs = [ ];
            };

          });
      in
      {
        devShell = project.shells.ghc;
      });
}
