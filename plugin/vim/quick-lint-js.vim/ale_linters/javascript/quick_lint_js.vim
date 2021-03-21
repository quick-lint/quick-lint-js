" Copyright (C) 2020  Matthew Glazar
" See end of file for extended copyright information.

" quick-lint-js registration for ALE - Asynchronous Lint Engine.
" https://github.com/dense-analysis/ale

call ale#Set('javascript_quick_lint_js_executable', 'quick-lint-js')
call ale#Set('javascript_quick_lint_js_use_global', get(g:, 'ale_use_global_executables', 0))

if ale#Has('ale-2.4.0')
  let s:linter_command_callback_key = 'command'
  let s:linter_executable_callback_key = 'executable'
else
  " The _callback variants are not supported since ALE v3.0.0. For details, see
  " "Implement a uniform API for asynchronous processing for most ALE features":
  " https://github.com/dense-analysis/ale/issues/2132
  let s:linter_command_callback_key = 'command_callback'
  let s:linter_executable_callback_key = 'executable_callback'
endif

call ale#linter#Define('javascript', {
  \ 'aliases': ['quick_lint_js', 'quicklintjs'],
  \ 'callback': function('quick_lint_js_ale#parse_command_output'),
  \ 'name': 'quick-lint-js',
  \ 'output_stream': 'stdout',
  \ 'read_buffer': 0,
  \ s:linter_command_callback_key: function('quick_lint_js_ale#get_command'),
  \ s:linter_executable_callback_key: function('quick_lint_js_ale#get_executable'),
\ })

" quick-lint-js finds bugs in JavaScript programs.
" Copyright (C) 2020  Matthew Glazar
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
