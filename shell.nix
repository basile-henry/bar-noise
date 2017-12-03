{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc821", doBenchmark ? false }:
(nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./. {}).env
