{ inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/24.05";

    utils.url = "github:numtide/flake-utils/v1.0.0";
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        config = { };

        overlay = pkgsNew: pkgsOld: {
          rumor =
            pkgsNew.haskell.lib.justStaticExecutables
              pkgsNew.haskellPackages.rumor;

          haskellPackages = pkgsOld.haskellPackages.override (old: {
            overrides = pkgsNew.haskell.lib.packageSourceOverrides {
              rumor = ./.;
            };
          });
        };

        pkgs =
          import nixpkgs { inherit config system; overlays = [ overlay ]; };

      in
        rec {
          packages.default = pkgs.haskellPackages.rumor;

          apps.default = {
            type = "app";

            program = "${pkgs.rumor}/bin/rumor";
          };

          devShells.default = pkgs.haskellPackages.rumor.env;
        }
    );
}
