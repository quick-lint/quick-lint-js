-- Copyright (C) 2020  Matthew Glazar
-- See end of file for extended copyright information.
--
-- quick-lint-js registration for nvim-lspconfig - Collection of common
-- configurations for Neovim's built-in language server client.
-- https://github.com/neovim/nvim-lspconfig

local lspconfig = require("lspconfig")
local lspconfig_configs = require("lspconfig/configs")
local lspconfig_util = require("lspconfig/util")

lspconfig_configs.quick_lint_js = {
  default_config = {
    cmd = {"quick-lint-js", "--lsp-server"},
    filetypes = {"javascript"},
    root_dir = function(fname)
      local root = lspconfig_util.path.dirname(fname)
      lspconfig_util.path.traverse_parents(fname, function(dir, _path)
        root = dir
      end)
      return root
    end
  },
  docs = {
    description = [[
https://quick-lint-js.com/

quick-lint-js finds bugs in JavaScript programs.
[200× faster than ESLint](https://quick-lint-js.com/benchmarks/), quick-lint-js gives you instant feedback as you code.
Find bugs in your JavaScript before your finger leaves the keyboard.
Lint any JavaScript file with no configuration.

See https://quick-lint-js.com/install/ for the installation documentation.
]],
    default_config = {
      root_dir = "/",
    }
  }
}

-- Allow users to write:
--   require("lspconfig/quick_lint_js").setup {}
return lspconfig.quick_lint_js

-- quick-lint-js finds bugs in JavaScript programs.
-- Copyright (C) 2020  Matthew Glazar
--
-- This file is part of quick-lint-js.
--
-- quick-lint-js is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- quick-lint-js is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with quick-lint-js.  If not, see <https://www.gnu.org/licenses/>.
