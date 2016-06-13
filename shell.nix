{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc801" }:

let

  inherit (nixpkgs) pkgs;

  f = import ./default.nix;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = pkgs.haskell.lib.addBuildTools
    (haskellPackages.callPackage f {})
    (with haskellPackages; [ cabal-install hscolour ]);

in

  if pkgs.lib.inNixShell then drv.env else drv
