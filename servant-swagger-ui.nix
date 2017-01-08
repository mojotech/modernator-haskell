{ mkDerivation, aeson, base, base-compat, blaze-markup, bytestring
, directory, file-embed, filepath, http-media, lens, servant
, servant-blaze, servant-server, servant-swagger, stdenv, swagger2
, template-haskell, text, transformers, transformers-compat, wai
, wai-app-static, warp
}:
mkDerivation {
  pname = "servant-swagger-ui";
  version = "0.2.1.2.2.8";
  sha256 = "195kmk2izws58mzkb1qmpvkk91kd249dybmf2jd8ayjjqgsmv8i1";
  libraryHaskellDepends = [
    base blaze-markup bytestring directory file-embed filepath
    http-media servant servant-blaze servant-server servant-swagger
    swagger2 template-haskell text transformers transformers-compat
    wai-app-static
  ];
  testHaskellDepends = [
    aeson base base-compat blaze-markup bytestring directory file-embed
    filepath http-media lens servant servant-blaze servant-server
    servant-swagger swagger2 template-haskell text transformers
    transformers-compat wai wai-app-static warp
  ];
  homepage = "https://github.com/phadej/servant-swagger-ui#readme";
  description = "Servant swagger ui";
  license = stdenv.lib.licenses.bsd3;
  doHaddock = false;
}
