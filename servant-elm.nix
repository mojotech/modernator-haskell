{ mkDerivation, aeson, base, data-default, directory, elm-export
, fetchgit, hspec, interpolate, lens, mockery, process, servant
, servant-foreign, stdenv, text
}:
mkDerivation {
  pname = "servant-elm";
  version = "0.1.0.2";
  src = fetchgit {
    url = "https://github.com/mattjbray/servant-elm.git";
    sha256 = "04b83434naglhf8hf34qvpsgyg89kbc103qk1ckk48xkirqkzajh";
    rev = "e13c8def8127ea339e9801d804638854947193e8";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base elm-export lens servant servant-foreign text
  ];
  executableHaskellDepends = [ base elm-export servant ];
  testHaskellDepends = [
    aeson base data-default directory elm-export hspec interpolate
    mockery process servant
  ];
  homepage = "http://github.com/mattjbray/servant-elm#readme";
  description = "Automatically derive Elm functions to query servant webservices";
  license = stdenv.lib.licenses.bsd3;
}

