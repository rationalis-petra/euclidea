# fhsUser.nix
{ pkgs ? import <nixpkgs> {} }:
(pkgs.buildFHSUserEnv {
  name = "sbcl-env";
  targetPkgs = pkgs: with pkgs; [
    coreutils
  ];
  multiPkgs = pkgs: with pkgs; [
    git
    sbcl
    glfw
    libffi
    libGL
  ];
  runScript = "bash";
}).env
