{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
          #hakyll  # list of Haskell packages here!
          JuicyPixels
          cabal-install  # for now... Nix's highlighting-kate unicode broken
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "my-hakyll-env";
  buildInputs = [ ghc pkgs.zlib ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
