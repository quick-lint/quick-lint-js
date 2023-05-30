# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

[scriptblock]$ScriptBlock = {
    param( $WordToComplete )

    $Options = @(
        @{
            CompletionText = '--help'
            ToolTip = 'Print a help message and exit'
        },
        @{
            CompletionText = '--version'
            ToolTip = 'Print version information and exit'
        },
        @{
            CompletionText = '--lsp-server'
            ToolTip = 'Run quick-lint-js in LSP server mode'
        },
        @{
            CompletionText = '--config-file='
            ToolTip = 'Read configuration options from file and apply them to input files which are given later in the command line'
        },
        @{
            CompletionText = '--stdin'
            ToolTip = 'Read standard input as a JavaScript file'
        },
        @{
            CompletionText = '--exit-fail-on='
            ToolTip = 'Fail with a non-zero exit code if any of these errors are found (default: all)'
        },
        @{
            CompletionText = '--output-format='
            ToolTip = "Customize how errors are printed`n`nvim-qflist-json: machine-readable JSON which can be given to Vim's setqflist function`ngnu-like: a human-readable format similar to GCC`nemacs-lisp: Emacs Lisp association list format"
        },
        @{
            CompletionText = '--diagnostic-hyperlinks='
            ToolTip = "Control whether to hyperlink error codes or not`n`nauto: shows error codes as hyperlinks only if the error output is a terminal`nalways: always shows error codes as hyperlinks`nnever: never shows error codes as hyperlinks"
        },
        @{
            CompletionText = '--snarky'
            ToolTip = 'Add spice to your failures'
        },
        @{
            CompletionText = '--vim-file-bufnr='
            ToolTip = 'Select a vim buffer for outputting feedback'
        },
        @{
            CompletionText = '--path-for-config-search'
            ToolTip = "For the input file or --stdin, use path as the file's path"
        },
        @{
            CompletionText = '--language'
            ToolTip = "Interpret input files which are given later in the command line as if they were written in languageid`n`ndefault: infer the languageid from the file’s extension`njavascript: the latest ECMAScript standard with proposed features`njavascript-jsx: like javascript but with JSX (React) extensions`nexperimental-typescript: the latest TypeScript version. (EXPERIMENTAL. Subject to change in future versions of quick-lint-js.)`nexperimental-typescript-jsx: like experimental-typescript but with JSX (React) extensions. (EXPERIMENTAL. Subject to change in future versions of quick-lint-js.)"
        },
        @{
            CompletionText = '--debug-apps'
            ToolTip = 'Print a list of running quick-lint-js instances which have the debug app enabled'
        }

    )

    $Options.Where({$_.CompletionText -like "$wordToComplete*"}) | ForEach-Object {
        [System.Management.Automation.CompletionResult]::new($_.CompletionText, $_.CompletionText, 'ParameterValue', $_.ToolTip)
    }
}

Register-ArgumentCompleter -CommandName quick-lint-js -Native -ScriptBlock $ScriptBlock

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
