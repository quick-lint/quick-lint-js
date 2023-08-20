" Copyright (C) 2020  Matthew "strager" Glazar
" See end of file for extended copyright information.

" :source this file in your .vimrc to have Vim pre-fill the contents of new
" files. See README.md for more details.

autocmd BufNewFile test-*.cpp call <SID>cpp_test_template()
autocmd BufNewFile *.cpp call <SID>cpp_template()
autocmd BufNewFile *.h call <SID>h_template()

function! s:cpp_template() abort
  if line('.') > 1
    return
  endif
  call s:load_template('new.cpp')

  let l:path = substitute(expand('%'), '\\', '/', 'g')
  " l:included_path excludes 'quick-lint-js/' and '.h' which are already in the
  " template.
  let l:included_path = matchstr(l:path, '\v(quick-lint-js/)?\zs(([^/]+/)?[^/]+)\ze\.cpp$')

  silent execute '4s@FILENAME@'.l:included_path.'@'
  7
endfunction

function! s:cpp_test_template() abort
  if line('.') > 1
    return
  endif
  call s:load_template('test-new.cpp')
  " Move the cursor after 'TEST(Test_' so the user can write the test suite's
  " name.
  normal! 8G0f,
endfunction

function! s:h_template() abort
  if line('.') > 1
    return
  endif
  call s:load_template('new.h')
  7
endfunction

function! s:load_template(name) abort
  execute 'read ++edit tools/templates/' . a:name
  " :read inserts the text after a blank line. Delete the blank line.
  1
  normal! dd
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
