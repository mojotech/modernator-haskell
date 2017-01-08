{ nixpkgsFn ? import ./nixpkgs.nix
, compiler ? "ghc801"
, system ? null }:
let haskellPackages = nixpkgs.pkgs.haskell.packages.${compiler};
    callPackage = haskellPackages.callPackage;
    nixpkgs = nixpkgsFn ({
      # extra config goes here
    } // ( if system == null then {} else { inherit system; } ));
    packageOverrides = rec {
      servant-aeson-specs = lib.dontCheck haskellPackages.servant-aeson-specs;
      # This version has a removeSession function
      servant-auth-cookie = callPackage ./servant-auth-cookie.nix {};
      # There's a bug in haddock that needs fixing, until then these are necessary
      # see https://github.com/haskell/haddock/issues/508, should be fixed with ghc802
      swagger2 = lib.dontHaddock haskellPackages.swagger2;
      servant-swagger = callPackage ./servant-swagger.nix { swagger2 = swagger2; };
      servant-swagger-ui = callPackage ./servant-swagger-ui.nix { servant-swagger = servant-swagger; swagger2 = swagger2; };
    };
    lib = nixpkgs.pkgs.haskell.lib;
in
callPackage ./modernator-haskell.nix ({
  buildTools = [ nixpkgs.pkgs.haskellPackages.cabal-install ];
} // packageOverrides)
