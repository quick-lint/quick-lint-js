# Copyright (C) 2020  Matthew Glazar
# See end of file for extended copyright information.

# This file is a script for the Nix package manager. This file is at the root of
# the repository so the Nix CLI can easily install quick-lint-js from a source
# tarball.

{ pkgs ? import <nixpkgs> { }, ... }:

{
  quick-lint-js = pkgs.callPackage ./dist/nix/quick-lint-js.nix { };

  # TODO: add expression for vim plugin
}

# quick-lint-js finds bugs in JavaScript programs.
# Copyright (C) 2020  Matthew Glazar
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
