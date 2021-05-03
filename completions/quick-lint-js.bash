#/usr/bin/env bash

# Copyright (C) 2020  Matthew Glazar
# See end of file for extended copyright information.

_quick-lint-js () {
        local cur prev opts

        COMPREPLY=()
        cur="${COMP_WORDS[COMP_CWORD]}"
        prev="${COMP_WORDS[COMP_CWORD-1]}"
        opts="-h --help -v --version --lsp --lsp-server --exit-fail-on --output-format --vim-file-bufnr"

        if [[ $cur == -* ]]; then
                COMPREPLY=($(compgen -W '${opts}' -- "$cur"))
                [[ ${COMPREPLY-} == *= ]] && compopt -o nospace
                return 0
        fi
}

complete -o default -F _quick-lint-js quick-lint-js

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
