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

        # Tools simplex shells out to at runtime. simplex invokes these by
        # bare name (pdflatex/xelatex/latexmk/lwarpmk/pdfcrop from texlive,
        # dot from graphviz, gnuplot, and convert from imagemagick), so they
        # must be present on PATH when the produced binary runs.
        runtimeDeps = [
          pkgs.texlive.combined.scheme-full
          pkgs.graphviz
          pkgs.gnuplot
          pkgs.imagemagick
        ];

        # callCabal2nix reads simplex.cabal directly, so dependencies stay
        # in sync with the cabal file automatically.
        simplexUnwrapped = pkgs.haskell.lib.overrideCabal
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

        # Wrap the binary so its runtime tools are always on PATH,
        # regardless of the user's environment.
        simplex = pkgs.runCommand "simplex-${simplexUnwrapped.version}"
          {
            nativeBuildInputs = [ pkgs.makeWrapper ];
            inherit (simplexUnwrapped) meta;
          }
          ''
            mkdir -p $out/bin
            makeWrapper ${simplexUnwrapped}/bin/simplex $out/bin/simplex \
              --prefix PATH : ${pkgs.lib.makeBinPath runtimeDeps}
          '';
      in
      {
        packages = {
          simplex = simplex;
          simplex-unwrapped = simplexUnwrapped;
          default = simplex;
        };

        apps.default = {
          type = "app";
          program = "${simplex}/bin/simplex";
        };

        devShells.default = haskellPackages.shellFor {
          packages = _: [ simplexUnwrapped ];
          nativeBuildInputs = with haskellPackages; [
            cabal-install
            ghc
          ] ++ runtimeDeps;
        };
      });
}
