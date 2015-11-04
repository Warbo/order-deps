{ mkDerivation, aeson, base, bytestring, containers, HS2AST, stdenv
, tasty, tasty-quickcheck, text
}:
mkDerivation {
  pname = "order-deps";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers HS2AST text
  ];
  testHaskellDepends = [
    aeson base bytestring containers HS2AST tasty tasty-quickcheck text
  ];
  homepage = "https://github.com/ouanixi/order-deps.git";
  description = "Toplogical sort of AST's as per their dependencies";
  license = stdenv.lib.licenses.gpl3;
}
