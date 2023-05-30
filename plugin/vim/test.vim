" Copyright (C) 2020  Matthew "strager" Glazar
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
  call s:test_parse_command_output()
  call s:test_buffer_is_associated_with_file()
endfunction

function! s:test_parse_command_output() abort
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

function! s:test_buffer_is_associated_with_file() abort
  %bwipeout!
  call assert_false(quick_lint_js_ale#is_buffer_associated_with_file(bufnr('%')))

  %bwipeout!
  silent edit file-name.txt
  call assert_true(quick_lint_js_ale#is_buffer_associated_with_file(bufnr('%')))

  %bwipeout!
  silent edit file-name.txt
  set buftype=nofile
  call assert_false(quick_lint_js_ale#is_buffer_associated_with_file(bufnr('%')))

  %bwipeout!
  help
  set filetype=javascript
  call assert_false(quick_lint_js_ale#is_buffer_associated_with_file(bufnr('%')))

  " Check a buffer different from the current buffer:
  %bwipeout!
  silent edit file.txt
  let l:file_buffer_number = bufnr('%')
  new
  call assert_false(quick_lint_js_ale#is_buffer_associated_with_file(bufnr('%')))
  call assert_true(quick_lint_js_ale#is_buffer_associated_with_file(l:file_buffer_number))
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
