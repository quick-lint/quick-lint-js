" quick-lint-js helper functions for ALE - Asynchronous Lint Engine.
" https://github.com/dense-analysis/ale

function! quick_lint_js_ale#get_command(buffer_number) abort
  return '%e --output-format=vim-qflist-json --vim-file-bufnr '.string(a:buffer_number).' %t'
endfunction

function! quick_lint_js_ale#get_executable(buffer_number) abort
  return ale#Var(a:buffer_number, 'javascript_quick_lint_js_executable')
endfunction

function! quick_lint_js_ale#parse_command_output(buffer, lines) abort
  let l:json = join(a:lines, "\n")
  if l:json ==# ''
    return []
  endif
  try
    let l:data = json_decode(l:json)
  catch /^Vim(let):E491:/
    " We possibly received partial JSON. Try to complete it.
    let l:data = json_decode(l:json.']}')
  endtry
  return l:data.qflist
endfunction
