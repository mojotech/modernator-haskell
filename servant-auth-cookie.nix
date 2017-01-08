{ mkDerivation, base, base-compat, base64-bytestring, blaze-builder
, blaze-html, blaze-markup, bytestring, cereal, cookie, cryptonite
, data-default, deepseq, exceptions, hspec, http-api-data
, http-media, http-types, memory, mtl, QuickCheck, servant
, servant-blaze, servant-server, stdenv, tagged, text, time
, transformers, wai, warp
}:
mkDerivation {
  pname = "servant-auth-cookie";
  version = "0.4.2";
  sha256 = "0ygdl693243lnq1sgphyls6cldng3lwx4rsmffrz1m9kv8bra6g1";
  libraryHaskellDepends = [
    base base64-bytestring blaze-builder bytestring cereal cookie
    cryptonite data-default exceptions http-api-data http-types memory
    mtl servant servant-server tagged time transformers wai
  ];
  testHaskellDepends = [
    base base-compat blaze-html blaze-markup bytestring cereal
    cryptonite data-default deepseq hspec http-api-data http-media mtl
    QuickCheck servant servant-blaze servant-server text time wai warp
  ];
  description = "Authentication via encrypted cookies";
  license = stdenv.lib.licenses.bsd3;
}

