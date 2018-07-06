# { stdenv, pkgs, haskell }:
with import <nixpkgs> { };

stdenv.mkDerivation rec {
  name = "source";
  buildInputs = [
    haskell.compiler.ghc822
    pkgs.cabal-install
  ];
}
