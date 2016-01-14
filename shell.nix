{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
          hakyll  # list of Haskell packages here!
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "my-hakyll-env";
  buildInputs = [ ghc pkgs.zlib ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
