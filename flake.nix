{
  inputs.flake-utils.url = github:numtide/flake-utils;
  outputs = { self, nixpkgs, flake-utils }:
  flake-utils.lib.eachSystem flake-utils.lib.allSystems (system:
  let pkgs = nixpkgs.legacyPackages.${system};
  in rec {
    # Runs with nix build .#quick-lint-js
    packages.quick-lint-js = pkgs.callPackage ./dist/nix/quick-lint-js.nix { };
    packages.vimPlugin = pkgs.vimUtils.buildVimPlugin {
      name = "quick-lint-js";
      src = plugin/vim/quick-lint-js.vim;
      meta = packages.quick-lint-js.meta;
    };
    # Runs with nix build
    packages.default = packages.quick-lint-js;

  }) // rec {
    # Used by `nix flake init -t <flake>`
    templates.simple-neovim = { 
      description = "simple neovim with quick-lint-js activated in it"; 
      path = ./dist/nix/templates/simple-neovim; 
    };
    templates.default = templates.simple-neovim;
  };
}
