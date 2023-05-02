# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

# This file is a script for the Nix package manager. This file is at the root of
# the repository so the Nix CLI can easily install quick-lint-js from a source
# tarball.
#
# Usage:
#
# $ # Run the quick-lint-js CLI on hello.js:
# $ nix run . -- hello.js
#
# $ # Run Neovim with quick-lint-js installed:
# $ nix run .#simple-neovim-with-qljs

{
  inputs.flake-utils.url = github:numtide/flake-utils;
  outputs = { self, nixpkgs, flake-utils }:
  flake-utils.lib.eachSystem flake-utils.lib.defaultSystems (system:
  let pkgs = nixpkgs.legacyPackages.${system};
  in rec {
    # Runs with nix build .#quick-lint-js
    packages.quick-lint-js = pkgs.callPackage ./dist/nix/quick-lint-js.nix { };
    packages.vimPlugin = pkgs.vimUtils.buildVimPlugin {
      name = "quick-lint-js";
      src = plugin/vim/quick-lint-js.vim;
      meta = packages.quick-lint-js.meta;
    };

    packages.simple-neovim-with-qljs = pkgs.neovim.override {
      configure = {
        customRC = ''
          let $PATH = $PATH.":${packages.quick-lint-js}/bin"
        '';
        packages.myPackages = {
          start = with pkgs.vimPlugins;[
            # loaded on launch
              ale
              packages.vimPlugin 
            ];
            # manually loadable by calling `:packadd $plugin-name`
            opt = [ ];
        };
      };
    };
    # Runs with nix build
    packages.default = packages.quick-lint-js;

  });
}

# quick-lint-js finds bugs in JavaScript programs.
# Copyright (C) 2020  Matthew "strager" Glazar
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
