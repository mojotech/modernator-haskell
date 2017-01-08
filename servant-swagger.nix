{ mkDerivation, aeson, aeson-qq, base, bytestring, directory
, doctest, filepath, hspec, http-media, insert-ordered-containers
, lens, QuickCheck, servant, stdenv, swagger2, text, time
, unordered-containers
}:
mkDerivation {
  pname = "servant-swagger";
  version = "1.1.2";
  sha256 = "0zn4r325mp1aqcg6gq2lvwizvypaabqdsnsx5v8gx29117gmr98s";
  revision = "2";
  editedCabalFile = "1b4ce7e527556edbd2f07fe420fe82a482d7d2b6ce264606c2e34f0e0270c081";
  libraryHaskellDepends = [
    aeson base bytestring hspec http-media insert-ordered-containers
    lens QuickCheck servant swagger2 text unordered-containers
  ];
  testHaskellDepends = [
    aeson aeson-qq base directory doctest filepath hspec lens
    QuickCheck servant swagger2 text time
  ];
  homepage = "https://github.com/haskell-servant/servant-swagger";
  description = "Generate Swagger specification for your servant API";
  license = stdenv.lib.licenses.bsd3;
  doHaddock = false;
}

