{ nixpkgsFn ? import ./nixpkgs.nix
, compiler ? "ghc7102"
, system ? null }:
let haskellPackages = nixpkgs.pkgs.haskell.packages.${compiler};
    callPackage = haskellPackages.callPackage;
    nixpkgs = nixpkgsFn ({
      # extra config goes here
    } // ( if system == null then {} else { inherit system; } ));
    packageOverrides = {
      servant-aeson-specs = lib.dontCheck haskellPackages.servant-aeson-specs;
      servant-auth-cookie = callPackage ./servant-auth-cookie.nix {};
    };
    lib = nixpkgs.pkgs.haskell.lib;
in
callPackage ./modernator-haskell.nix ({
  buildTools = [ nixpkgs.pkgs.haskellPackages.cabal-install ];
} // packageOverrides)
