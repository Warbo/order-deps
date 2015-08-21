{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring
      , directory, QuickCheck, stdenv, stringable
      , tasty, tasty-quickcheck
      }:
      mkDerivation {
        pname = "order-deps";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [
          base bytestring stringable
        ];
        testDepends = [
          base bytestring directory
          QuickCheck stringable tasty tasty-quickcheck
        ];
        homepage = "http://chriswarbo.net/essays/repos/tree-features.html";
        description = "Feature extraction for tree structured data";
        license = stdenv.lib.licenses.gpl3;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
