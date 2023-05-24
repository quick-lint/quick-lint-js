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
# $ nix run .#neovim-nvim-lspconfig
#
# $ # Run Emacs with quick-lint-js installed:
# $ nix run .#emacs-flycheck
#
# $ # Test shell completions:
# $ nix run .#bash
# $ nix run .#fish
# $ nix run .#powershell
# $ nix run .#zsh

{
  inputs.flake-utils.url = github:numtide/flake-utils;

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem flake-utils.lib.defaultSystems (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in rec {
        # Runs with nix build .#quick-lint-js
        packages.quick-lint-js = pkgs.callPackage ./dist/nix/quick-lint-js.nix { };

        packages.emacsPlugin = let epkgs = pkgs.emacs.pkgs; in epkgs.trivialBuild {
          pname = "quick-lint-js";
          src = plugin/emacs;
          nativeBuildInputs = [
            epkgs.eglot
            epkgs.flycheck
            epkgs.lsp-mode
            epkgs.flymake
          ];
          meta = packages.quick-lint-js.meta;
        };

        packages.vimPlugin = pkgs.vimUtils.buildVimPlugin {
          name = "quick-lint-js";
          src = plugin/vim/quick-lint-js.vim;
          meta = packages.quick-lint-js.meta;
        };

        # Bash shell for testing completions.
        packages.bash = let
          bashInit = pkgs.runCommand "bashInit" {
            buildInputs = [ pkgs.bash-completion packages.quick-lint-js ];
          } ''
            {
              export
              printf ". ${pkgs.bash-completion}/share/bash-completion/bash_completion\n"
              printf ". ${packages.quick-lint-js}/share/bash-completion/completions/quick-lint-js.bash\n"
            } >"$out"
          '';
          in pkgs.writeShellScriptBin "quick-lint-js-bash" ''
            ${pkgs.bashInteractive}/bin/bash --init-file ${bashInit}
          '';

        # Fish shell for testing completions.
        packages.fish = pkgs.writeShellScriptBin "quick-lint-js-fish" ''
          ${pkgs.fish}/bin/fish \
            --no-config \
            --init-command='set PATH ${packages.quick-lint-js}/bin $PATH' \
            --init-command='set fish_complete_path ${packages.quick-lint-js}/share/fish/vendor_completions.d/ $fish_complete_path'
        '';

        # PowerShell shell for testing completions.
        packages.powershell = pkgs.writeShellScriptBin "quick-lint-js-powershell" ''
          ${pkgs.powershell}/bin/pwsh \
            -NoExit \
            -NoLogo \
            -NoProfile \
            -Command '
              . ${packages.quick-lint-js}/share/powershell/completions/quick-lint-js.ps1
              $env:PATH = "${packages.quick-lint-js}/bin:" + $env:PATH
            '
        '';

        # Zsh shell for testing completions.
        packages.zsh = let
          zshDotDir = pkgs.runCommand "zshDotDir" {
            buildInputs = [ packages.quick-lint-js ];
          } ''
            mkdir "$out"
            {
              export
              printf 'fpath+=${packages.quick-lint-js}/share/zsh/site-functions\n'
              printf 'autoload -Uz compinit\n'
              printf 'compinit\n'
            } >"$out/.zshrc"
          '';
          in pkgs.writeShellScriptBin "quick-lint-js-zsh" ''
            ZDOTDIR=${zshDotDir} ${pkgs.zsh}/bin/zsh
          '';

        packages.simple-neovim-with-qljs = pkgs.neovim.override {
          configure = {
            customRC = ''
              let $PATH = $PATH.":${packages.quick-lint-js}/bin"
            '';
            packages.myPackages = {
              start = with pkgs.vimPlugins; [
                  # loaded on launch
                  ale
                  packages.vimPlugin
                ];
                # manually loadable by calling `:packadd $plugin-name`
                opt = [ ];
            };
          };
        };

        # Neovim configured with quick-lint-js and nvim-lspconfig for testing.
        packages.neovim-nvim-lspconfig = pkgs.neovim.override {
          configure = {
            customRC = ''
              lua << EOF
              require('lspconfig/quick_lint_js').setup {
                cmd = {"${packages.quick-lint-js}/bin/quick-lint-js", "--lsp-server"},
              }
              EOF
            '';
            packages.myPackages = {
              start = [
                pkgs.vimPlugins.nvim-lspconfig
                packages.vimPlugin
              ];
            };
          };
        };

        # Create a custom version of Emacs with the given list of packages and
        # elisp settings.
        makeCustomEmacsWith = { emacsConfig, packages }:
          let
            emacsConfigFile = pkgs.writeText "default.el" emacsConfig;
            emacsConfigPackage = pkgs.runCommand "default.el" {} ''
              mkdir -p $out/share/emacs/site-lisp
              cp ${emacsConfigFile} $out/share/emacs/site-lisp/default.el
            '';
            emacs = pkgs.emacs.pkgs.withPackages (epkgs: packages epkgs ++ [ emacsConfigPackage ]);
          in pkgs.writeShellScriptBin "emacs-custom" "exec ${emacs}/bin/emacs \"\${@}\"";

        # Emacs configured with quick-lint-js and Flycheck for testing.
        packages.emacs-flycheck = makeCustomEmacsWith {
          packages = epkgs: [
            packages.emacsPlugin
            epkgs.flycheck
          ];
          emacsConfig = ''
            (require 'flycheck-quicklintjs)

            (defun my-flycheck-quicklintjs-setup ()
              "Configure flycheck-quicklintjs."

              (unless (bound-and-true-p flycheck-mode)
                (flycheck-mode))
              (flycheck-select-checker 'javascript-quicklintjs)
              (setq-local flycheck-idle-change-delay 0)
              (setq-local flycheck-check-syntax-automatically '(mode-enabled idle-change new-line)))

              (custom-set-variables
               '(flycheck-javascript-quicklintjs-executable "${packages.quick-lint-js}/bin/quick-lint-js"))
            (add-hook 'js-mode-hook #'my-flycheck-quicklintjs-setup)
          '';
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
