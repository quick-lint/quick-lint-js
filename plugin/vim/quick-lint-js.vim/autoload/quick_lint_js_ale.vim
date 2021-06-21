" Copyright (C) 2020  Matthew "strager" Glazar
" See end of file for extended copyright information.

" quick-lint-js helper functions for ALE - Asynchronous Lint Engine.
" https://github.com/dense-analysis/ale

function! quick_lint_js_ale#get_command(buffer_number) abort
  return '%e --output-format=vim-qflist-json --vim-file-bufnr '.string(a:buffer_number).' %t'
endfunction

function! quick_lint_js_ale#get_executable(buffer_number) abort
  return ale#node#FindExecutable(a:buffer_number, 'javascript_quick_lint_js', [
    \ 'node_modules/.bin/quick-lint-js',
  \ ])
endfunction

function! quick_lint_js_ale#parse_command_output(buffer, lines) abort
  let l:json = join(a:lines, "\n")
  if l:json ==# ''
    return []
  endif
  try
    let l:data = json_decode(l:json)
  catch /^Vim(let):\(E491\|E474\):/
    " We possibly received partial JSON. Try to complete it.
    let l:data = json_decode(l:json.']}')
  endtry
  return l:data.qflist
endfunction

function! quick_lint_js_ale#get_lsp_command(_buffer_number) abort
  return '%e --lsp-server'
endfunction

function! quick_lint_js_ale#get_lsp_project_root(_buffer_number) abort
  return '/'
endfunction

" quick-lint-js finds bugs in JavaScript programs.
" Copyright (C) 2020  Matthew "strager" Glazar
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
