{ pkgs
, stdenv ? pkgs.stdenv
, lib ? stdenv.lib
, mkDerivation ? stdenv.mkDerivation

, buildType ? "Release"
, doCheck ? true
, dontFixCmake ? false
}:

mkDerivation {
  pname = "quick-lint-js";
  # nix conventions tell us to use a version string starting with
  # a number or the "current" date. Since this is a rolling derivation
  # we can't give a date, and we currently have no version numbers.
  # FIXME as soon as we introduce version numbers.
  version = "0";

  src = ../.;
  unpackPhase = null;

  nativeBuildInputs = with pkgs; [ cmake ninja ];
  cmakeBuildType = buildType;
  doCheck = doCheck;
  dontFixCmake = dontFixCmake;

  meta = with lib; {
    description = "quick-lint-js finds bugs in JavaScript programs";
    homepage = "https://github.com/strager/quick-lint-js";
    license = licenses.gpl3Plus;
    platforms = platforms.all;
  };
}
