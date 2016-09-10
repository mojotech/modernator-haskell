{ nixpkgsFn ? import ./nixpkgs.nix
, compiler ? "ghc7102"
, system ? null }:
let haskellPackages = nixpkgs.pkgs.haskell.packages.${compiler};
    callPackage = haskellPackages.callPackage;
    nixpkgs = nixpkgsFn ({
      # extra config goes here
    } // ( if system == null then {} else { inherit system; } ));
    packageOverrides = rec {
      # these tests fail due to file access permissions
      servant-aeson-specs = lib.dontCheck haskellPackages.servant-aeson-specs;
      servant-elm = lib.dontCheck (callPackage ./servant-elm.nix  {
        elm-export = lib.dontCheck (callPackage ./elm-export.nix {});
      });
    };
    lib = nixpkgs.pkgs.haskell.lib;
in
callPackage ./modernator-haskell.nix ({
  buildTools = [ nixpkgs.pkgs.haskellPackages.cabal-install ];
} // packageOverrides)
