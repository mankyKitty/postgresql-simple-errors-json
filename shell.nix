{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, json-schema, postgresql-simple
      , stdenv, text
      }:
      mkDerivation {
        pname = "postgresql-simple-errors-json";
        version = "0.1.0.0";
        src = ./.;
        buildDepends = [ aeson base json-schema postgresql-simple text ];
        description = "Some Aeson and JSONSchema instances for postgresql-simple errors";
        license = stdenv.lib.licenses.bsd3;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
