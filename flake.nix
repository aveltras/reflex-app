{
  description = "Reflex App";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    flake-compat.url = github:edolstra/flake-compat;
    flake-compat.flake = false;
    flake-utils.url = "github:numtide/flake-utils";
    reflex-platform.url = "github:reflex-frp/reflex-platform/develop";
    reflex-platform.flake = false;
    reflex-gadt-api.url = "github:mgttlinger/reflex-gadt-api/develop";
    reflex-gadt-api.flake = false;
    aeson-gadt-th.url = "github:obsidiansystems/aeson-gadt-th/develop";
    aeson-gadt-th.flake = false;
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        reflexPlatform = import inputs.reflex-platform { inherit system; __useNewerCompiler = true; };
        pkgs = inputs.nixpkgs.legacyPackages."${system}".appendOverlays [
          inputs.self.overlay."${system}"
        ];
      in
      {
        overlay = final: prev: {
          ghc = reflexPlatform.ghc.override {
            overrides = hfinal: hprev: with final.haskell.lib; {
              aeson-gadt-th = hprev.callCabal2nix "aeson-gadt-th" inputs.aeson-gadt-th { };
              server = hprev.callCabal2nix "server" ./server { };
              reflex-dom = addBuildDepend (enableCabalFlag hprev.reflex-dom "use-warp") hfinal.jsaddle-warp;
              reflex-gadt-api = doJailbreak (hprev.callCabal2nix "reflex-gadt-api" inputs.reflex-gadt-api { });
            };
          };
          ghcjs = reflexPlatform.ghcjs;
        };

        devShell = pkgs.ghc.shellFor {
          packages = p: [ p.server ];
          buildInputs = [
            pkgs.cabal-install
            pkgs.dbmate
            pkgs.ghcjs.ghc
            pkgs.ghc.ghc
            pkgs.ghcid
            pkgs.haskell-language-server
            pkgs.pgcli
          ];
          shellHook = ''
            export PROJECT_DIRECTORY=$(pwd)
            export DBMATE_MIGRATIONS_DIR=$PROJECT_DIRECTORY/database/migrations
            export DBMATE_SCHEMA_FILE=$PROJECT_DIRECTORY/database/schema.sql
            export PGUSER=postgres
            export PGDATABASE=reflex-app
            export DATABASE_URL=postgres://$PGUSER/$PGDATABASE?host=/var/run/postgresql/
          '';
        };
      });
}
