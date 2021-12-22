{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.callPackage ./catrina.nix { }
