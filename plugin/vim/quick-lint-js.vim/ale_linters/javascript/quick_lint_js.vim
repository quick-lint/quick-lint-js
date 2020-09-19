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
