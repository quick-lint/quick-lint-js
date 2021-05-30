" Copyright (C) 2020  Matthew Glazar
" See end of file for extended copyright information.

" quick-lint-js registration for ALE - Asynchronous Lint Engine.
" https://github.com/dense-analysis/ale

call ale#Set('javascript_quick_lint_js_executable', 'quick-lint-js')
call ale#Set('javascript_quick_lint_js_use_global', get(g:, 'ale_use_global_executables', 0))

" TODO(strager): Make quick-lint-js-lsp the default when the bugs have been
" ironed out:
" https://github.com/quick-lint/quick-lint-js/issues/111
" https://github.com/quick-lint/quick-lint-js/issues/171
let s:enable_lsp_linter = v:false

if ale#Has('ale-2.4.0')
  let s:linter_command_callback_key = 'command'
  let s:linter_executable_callback_key = 'executable'
  let s:linter_project_root_callback_key = 'project_root'
else
  " The _callback variants are not supported since ALE v3.0.0. For details, see
  " "Implement a uniform API for asynchronous processing for most ALE features":
  " https://github.com/dense-analysis/ale/issues/2132
  let s:linter_command_callback_key = 'command_callback'
  let s:linter_executable_callback_key = 'executable_callback'
  let s:linter_project_root_callback_key = 'project_root_callback'
endif

if s:enable_lsp_linter
  call ale#linter#Define('javascript', {
    \ 'aliases': ['quick-lint-js-lsp', 'quick_lint_js', 'quicklintjs'],
    \ 'lsp': 'stdio',
    \ 'name': 'quick-lint-js',
    \ s:linter_command_callback_key: function('quick_lint_js_ale#get_lsp_command'),
    \ s:linter_executable_callback_key: function('quick_lint_js_ale#get_executable'),
    \ s:linter_project_root_callback_key: function('quick_lint_js_ale#get_lsp_project_root'),
  \ })
else
  call ale#linter#Define('javascript', {
    \ 'aliases': ['quick-lint-js-cli', 'quick_lint_js', 'quicklintjs'],
    \ 'callback': function('quick_lint_js_ale#parse_command_output'),
    \ 'name': 'quick-lint-js',
    \ 'output_stream': 'stdout',
    \ 'read_buffer': 0,
    \ s:linter_command_callback_key: function('quick_lint_js_ale#get_command'),
    \ s:linter_executable_callback_key: function('quick_lint_js_ale#get_executable'),
  \ })
endif

" quick-lint-js finds bugs in JavaScript programs.
" Copyright (C) 2020  Matthew Glazar
"
" This file is part of quick-lint-js.
"
" quick-lint-js is free software: you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation, either version 3 of the License, or
" (at your option) any later version.
"
" quick-lint-js is distributed in the hope that it will be useful,
" but WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
" GNU General Public License for more details.
"
" You should have received a copy of the GNU General Public License
" along with quick-lint-js.  If not, see <https://www.gnu.org/licenses/>.
