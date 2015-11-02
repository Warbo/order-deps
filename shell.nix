{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, containers, stdenv
      , tasty, tasty-quickcheck, text
      }:
      mkDerivation {
        pname = "order-deps";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base bytestring containers text
        ];
        testHaskellDepends = [
          aeson base bytestring containers tasty tasty-quickcheck text
        ];
        homepage = "https://github.com/ouanixi/order-deps.git";
        description = "Toplogical sort of AST's as per their dependencies";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
