" Copyright (C) 2020  Matthew Glazar
" See end of file for extended copyright information.

set nocompatible

function! s:main() abort
  try
    call s:test_all()
    call s:check_for_errors()
    qall!
  catch
    echomsg v:exception
    cquit
  endtry
endfunction

function! s:test_all() abort
  let l:qflist = s:parse([''])
  call assert_equal([], l:qflist)

  let l:qflist = s:parse(['{"qflist": []}'])
  call assert_equal([], l:qflist)

  let l:qflist = s:parse([
    \ '{"qflist": [{"col": 1, "lnum": 1, "end_col": 3, "end_lnum": 1, '
    \ . '"vcol": 0, "text": "let with no bindings", "bufnr": 1, '
    \ . '"filename": "/tmp/nvimpMqa35/4/get.js"}]}',
  \ ])
  call assert_equal([{
    \ "col": 1,
    \ "lnum": 1,
    \ "end_col": 3,
    \ "end_lnum": 1,
    \ "vcol": 0,
    \ "text": "let with no bindings",
    \ "bufnr": 1,
    \ "filename": "/tmp/nvimpMqa35/4/get.js",
  \ }], l:qflist)

  let l:qflist = s:parse(['{"qflist": ['])
  call assert_equal([], l:qflist)

  let l:qflist = s:parse([
    \ '{',
    \ '  "qflist": [',
    \ '    {',
    \ '      "col": 1,',
    \ '      "lnum": 1,',
    \ '      "end_col": 3,',
    \ '      "end_lnum": 1,',
    \ '      "vcol": 0,',
    \ '      "text": "let with no bindings",',
    \ '      "bufnr": 1,',
    \ '      "filename": "/tmp/nvimpMqa35/4/get.js"',
    \ '    }',
    \ '  ]',
    \ '}',
  \ ])
  call assert_equal([{
    \ "col": 1,
    \ "lnum": 1,
    \ "end_col": 3,
    \ "end_lnum": 1,
    \ "vcol": 0,
    \ "text": "let with no bindings",
    \ "bufnr": 1,
    \ "filename": "/tmp/nvimpMqa35/4/get.js",
  \ }], l:qflist)
endfunction

function! s:parse(lines) abort
  return quick_lint_js_ale#parse_command_output(0, a:lines)
endfunction

function! s:check_for_errors() abort
  if len(v:errors) > 0
    for l:error in v:errors
      echomsg l:error
    endfor
    cquit
  endif
endfunction

call s:main()

" quick-lint-js finds bugs in JavaScript programs.
" Copyright (C) 2020  Matthew Glazar
"
" This program is free software: you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation, either version 3 of the License, or
" (at your option) any later version.
"
" This program is distributed in the hope that it will be useful,
" but WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
" GNU General Public License for more details.
"
" You should have received a copy of the GNU General Public License
" along with this program.  If not, see <https://www.gnu.org/licenses/>.
