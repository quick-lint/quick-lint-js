# Copyright (C) 2020  Matthew Glazar
# See end of file for extended copyright information.

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
  version = "0.1.0";

  src = ../../.;
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

# quick-lint-js finds bugs in JavaScript programs.
# Copyright (C) 2020  Matthew Glazar
#
# This file is part of quick-lint-js.
#
# quick-lint-js is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# quick-lint-js is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with quick-lint-js.  If not, see <https://www.gnu.org/licenses/>.
