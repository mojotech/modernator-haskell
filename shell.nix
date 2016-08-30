{ nixpkgsFn ? import ./nixpkgs.nix, compiler ? "ghc7102" }:
(import ./default.nix { inherit nixpkgsFn compiler; }).env
