{ nixpkgsFn ? import ./nixpkgs.nix
, compiler ? "ghc7102"
, system ? null }:
let callPackage = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage;
    nixpkgs = nixpkgsFn ({
      # extra config goes here
    } // ( if system == null then {} else { inherit system; } ));
in
callPackage ./modernator-haskell.nix {
  buildTools = [ nixpkgs.pkgs.haskellPackages.cabal-install ];
}
