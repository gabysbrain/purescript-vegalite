{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  nodeEnv = import ./nix/node-env.nix {
    inherit (pkgs) stdenv python2 utillinux runCommand writeTextFile;
    inherit nodejs;
  };
  extraNodePkgs = import ./nix/node-packages.nix {
    inherit fetchurl fetchgit; 
    inherit nodeEnv;
  };
in
stdenv.mkDerivation {
  name = "purescript-monadicvis";

  buildInputs = [
    purescript
    nodejs
    nodePackages.bower
    extraNodePkgs.pulp
  ];
}

