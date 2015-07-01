{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./postgresql-simple-errors-json.nix { }
