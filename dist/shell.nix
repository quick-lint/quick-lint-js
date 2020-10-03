{ pkgs ? import <nixpkgs> { }
, buildType ? "Debug"
# Without disabling fixing, code in vendor/ will be modified when building
# interactively.
, dontFixCmake ? true
, ...
}:

{
  quick-lint-js = pkgs.callPackage ./quick-lint-js.nix {
    inherit pkgs buildType dontFixCmake;
  };
}
