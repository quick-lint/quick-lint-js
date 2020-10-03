{ pkgs ? import <nixpkgs> { }, ... }:

{
  quick-lint-js = pkgs.callPackage ./quick-lint-js.nix { };

  # TODO: add expression for vim plugin
}
