{
  description = "Simplex - a simple markup language that translates to LaTeX";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        haskellPackages = pkgs.haskellPackages;

        # callCabal2nix reads simplex.cabal directly, so dependencies stay
        # in sync with the cabal file automatically.
        simplex = pkgs.haskell.lib.overrideCabal
          (haskellPackages.callCabal2nix "simplex" ./. { })
          (_: {
            # The sources target an old mtl where Control.Monad.Cont
            # re-exported liftIO. Modern GHC/mtl no longer does, so bring
            # liftIO into scope explicitly without touching the tracked source.
            postPatch = ''
              substituteInPlace src/simplex.hs \
                --replace 'import Control.Monad.Cont' \
                          'import Control.Monad.Cont
import Control.Monad.IO.Class (liftIO)'
            '';
          });
      in
      {
        packages = {
          simplex = simplex;
          default = simplex;
        };

        apps.default = {
          type = "app";
          program = "${simplex}/bin/simplex";
        };

        devShells.default = haskellPackages.shellFor {
          packages = _: [ simplex ];
          nativeBuildInputs = with haskellPackages; [
            cabal-install
            ghc
          ];
        };
      });
}
