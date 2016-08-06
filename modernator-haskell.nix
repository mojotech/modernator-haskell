{ mkDerivation, acid-state, aeson, base, http-api-data, ixset, mtl
, safecopy, servant, servant-server, stdenv, text, warp, time
, servant-swagger, swagger2, servant-swagger-ui, either
, buildTools ? []
}:
mkDerivation {
  pname = "modernator-haskell";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    acid-state aeson base http-api-data ixset mtl safecopy servant
    servant-server text warp time
    servant-swagger swagger2 servant-swagger-ui either
  ];
  buildTools = buildTools;
  description = "An application for hosting targeted Q&A sessions like Reddit AMA's";
  license = stdenv.lib.licenses.mit;
}
