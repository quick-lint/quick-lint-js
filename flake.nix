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
