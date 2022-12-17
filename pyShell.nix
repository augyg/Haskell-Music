# shell.nix
{ pkgs ? import <nixpkgs> {} }:
let
  pythonEnv = pkgs.python3.withPackages (p: with p; [
    matplotlib     
  ]);
in
pkgs.mkShell {
  packages = [
    pythonEnv
  ];
}


