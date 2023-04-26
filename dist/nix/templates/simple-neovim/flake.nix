{
  #change url when merged
  inputs.quick-lint-js.url = github:countoren/quick-lint-js;
  inputs.flake-utils.url = github:numtide/flake-utils;

  outputs = { self, nixpkgs, flake-utils, quick-lint-js }:
  flake-utils.lib.eachDefaultSystem (system:
  let pkgs = nixpkgs.legacyPackages.${system};
  in rec {
    packages.default = pkgs.neovim.override {
      configure = {
        customRC = ''
          let $PATH = $PATH.":${quick-lint-js.packages.${system}.quick-lint-js}/bin"
        '';
        packages.myPackages = {
          start = with pkgs.vimPlugins;[
            # loaded on launch
              ale
              quick-lint-js.packages.${system}.vimPlugin 
            ];
            # manually loadable by calling `:packadd $plugin-name`
            opt = [ ];
        };
      };
    };
  });
}
