-- Copyright (C) 2020  Matthew Glazar
-- See end of file for extended copyright information.
--
-- quick-lint-js registration for nvim-lspconfig - Collection of common configurations for Neovim's built-in language server client.
-- https://github.com/neovim/nvim-lspconfig

local configs = require("lspconfig/configs")
local util = require("lspconfig/util")

configs.quick_lint_js = {
    default_config = {
        cmd = {"quick-lint-js", "--lsp-server"},
        filetypes = {"javascript"},
        root_dir = function(fname)
            return util.find_package_json_ancestor(fname) or
                       util.find_node_modules_ancestor(fname) or
                       util.find_git_ancestor(fname) or
                       util.path.dirname(fname)
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
            root_dir = [[root_pattern('package.json', 'node_modules', '.git') or dirname]]
        }
    }
}

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
