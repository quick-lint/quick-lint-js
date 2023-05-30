# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

complete -c quick-lint-js -l help -d 'Print help message' -f
complete -c quick-lint-js -l version -d 'Print version information' -f
complete -c quick-lint-js -l lsp-server -d 'Run quick-lint-js in LSP server mode' -r
complete -c quick-lint-js -l config-file -d 'Read configuration from a JSON file for later input files' -r
complete -c quick-lint-js -l stdin -d 'Read standard input as a JavaScript file' -r
complete -c quick-lint-js -l exit-fail-on -d 'Fail with a non-zero exit code if any of these errors are found (default: "all")' -r
complete -c quick-lint-js -l output-format -d 'Format to print feedback' -xa 'gnu-like\t"(default) a human-readable format similar to GCC" vim-qflist-json\t"machine-readable JSON which can be given to Vim\'s setqflist function" emacs-lisp\t"Emacs Lisp association list format"'
complete -c quick-lint-js -l diagnostic-hyperlinks -d 'Control whether to hyperlink error codes or not' -xa 'auto\t"(default) shows error codes as hyperlinks only if the error output is a terminal" always\t"always shows error codes as hyperlinks" never\t"never shows error codes as hyperlinks"'
complete -c quick-lint-js -l snarky -d 'Add spice to your failures' -r
complete -c quick-lint-js -l vim-file-bufnr -d 'Select a vim buffer for outputting feedback' -r

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

# vim: ft=sh:
