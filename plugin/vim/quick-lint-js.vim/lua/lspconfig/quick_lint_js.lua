-- Copyright (C) 2020  Matthew Glazar
-- See end of file for extended copyright information.
--
-- quick-lint-js registration for nvim-lspconfig - Collection of common
-- configurations for Neovim's built-in language server client.
-- https://github.com/neovim/nvim-lspconfig

local lspconfig = require("lspconfig")
local lspconfig_util = require("lspconfig/util")

local quick_lint_js_config = {
  default_config = {
    -- Keep these in sync with
    -- plugin/vim/quick-lint-js.vim/doc/quick-lint-js.txt.
    cmd = {"quick-lint-js", "--lsp-server"},
    filetypes = {"javascript", "javascriptreact", "typescript", "typescriptreact"},
    root_dir = function(fname)
      local root = lspconfig_util.path.dirname(fname)
      lspconfig_util.path.traverse_parents(fname, function(dir, _path)
        root = dir
      end)
      return root
    end,
    before_init = function(initialize_params, config)
      local qljs = require('quick-lint-js')
      qljs.nvim_lspconfig_update_initialization_options_from_settings(initialize_params, config.settings)
    end,
  },
  docs = {
    description = [[
https://quick-lint-js.com/

quick-lint-js finds bugs in JavaScript programs.
[160× faster than ESLint](https://quick-lint-js.com/benchmarks/), quick-lint-js gives you instant feedback as you code.
Find bugs in your JavaScript before your finger leaves the keyboard.
Lint any JavaScript file with no configuration.

See https://quick-lint-js.com/install/ for the installation documentation.
]],
    default_config = {
      root_dir = "/",
    }
  }
}

require("lspconfig.configs").quick_lint_js = quick_lint_js_config

-- HACK(strager): Since nvim-lspconfig commit 97da7ed12e [1] (November 25,
-- 2021), we must register with the module named "lspconfig.configs". Prior to
-- that commit, we must register with the module named "lspconfig/configs". This
-- is a breaking change in nvim-lspconfig [2].
--
-- There doesn't seem to be a way to detect whether we're on the old version or
-- the new version of nvim-lspconfig [3]. Register our config under both module
-- names. (require() succeeds with both module names both before and after the
-- breaking change.)
--
-- [1] https://github.com/neovim/nvim-lspconfig/commit/97da7ed12e7e0d86e735e38a8170e941d4ed3e9a
-- [2] https://github.com/neovim/nvim-lspconfig/issues/1075#issuecomment-980801779
-- [3] https://github.com/neovim/nvim-lspconfig/pull/1479#issuecomment-985950252
require("lspconfig/configs").quick_lint_js = quick_lint_js_config

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
