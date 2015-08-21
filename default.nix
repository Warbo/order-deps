{ mkDerivation, base, bytestring, directory
, parsec, QuickCheck, stdenv, stringable, tasty, aeson
, tasty-quickcheck
}:
mkDerivation {
  pname = "order-deps";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    attoparsec base bytestring parsec stringable aeson
  ];
  testDepends = [
    atto-lisp attoparsec base bytestring
    QuickCheck stringable tasty tasty-quickcheck
  ];
  homepage = "https://github.com/ouanixi/order-deps.git";
  description = "Topological sort of AST's by dependecy";
  license = stdenv.lib.licenses.gpl3;
}
