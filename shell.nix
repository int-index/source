# { stdenv, pkgs, haskell }:
with import <nixpkgs> { };

stdenv.mkDerivation rec {
  name = "source";
  buildInputs = [
    haskell.compiler.ghc843
    pkgs.cabal-install
  ];
}
