{ mkDerivation, base, containers, directory, fetchgit, hspec
, hspec-core, mtl, QuickCheck, quickcheck-instances, stdenv, text
, time
}:
mkDerivation {
  pname = "elm-export";
  version = "0.3.0.0";
  src = fetchgit {
    url = "https://github.com/mattjbray/elm-export.git";
    sha256 = "1kqzbvf6sair4jd72vi87gyg5a769rfp5sizdlhbpgf4flw5rx9f";
    rev = "8868c1f09597f44c2e18e014cd9fbcf8320c3fea";
  };
  libraryHaskellDepends = [
    base containers directory mtl text time
  ];
  testHaskellDepends = [
    base containers hspec hspec-core QuickCheck quickcheck-instances
    text time
  ];
  homepage = "http://github.com/krisajenkins/elm-export";
  description = "A library to generate Elm types, Decoders and Encoders from Haskell source, using Generics";
  license = "unknown";
}

