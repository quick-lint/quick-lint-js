# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

_quick_lint_js_output_formats="gnu-like vim-qflist-json emacs-lisp"
_quick_lint_js_diagnostic_hyperlink_modes="auto always never"
_quick_lint_js_languages="default javascript javascript-jsx experimental-typescript experimental-typescript-jsx"

_quick-lint-js () {
        local cur opts
        _init_completion -n = || return

        opts="--help --version --lsp-server --config-file= --stdin --exit-fail-on= --output-format= --diagnostic-hyperlinks= --vim-file-bufnr= --language= --path-for-config-search= --snarky --debug-apps"

        case $cur in
                --config-file=*)
                        _split_longopt
                        _filedir
                        return
                        ;;
                --output-format=*)
                        COMPREPLY=($(compgen -W "${_quick_lint_js_output_formats}" -- "${cur#*=}"))
                        return
                        ;;
                --diagnostic-hyperlinks=*)
                        COMPREPLY=($(compgen -W "${_quick_lint_js_diagnostic_hyperlink_modes}" -- "${cur#*=}"))
                        return
                        ;;
                --exit-fail-on=*|--vim-file-bufnr=*)
                        return
                        ;;
                --path-for-config-search=*)
                        _split_longopt
                        _filedir
                        return
                        ;;
                
                --language=*)
                        COMPREPLY=($(compgen -W "${_quick_lint_js_languages}" -- "${cur#*=}"))
                        return
                        ;;
        esac

        if [[ $cur == -* ]]; then
                COMPREPLY=($(compgen -W "${opts}" -- $cur))
                [[ ${COMPREPLY-} == *= ]] && compopt -o nospace
                return
        else
                _filedir
        fi
}

complete -F _quick-lint-js quick-lint-js

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
