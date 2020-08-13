set(QUICK_LINT_JS_CXX_COMPILER_OPTIONS "" CACHE STRING "")
mark_as_advanced(QUICK_LINT_JS_CXX_COMPILER_OPTIONS)

function (quick_lint_js_add_executable TARGET)
  add_executable(${ARGV})
  target_compile_options(
    "${TARGET}"
    PRIVATE
    "${QUICK_LINT_JS_CXX_COMPILER_OPTIONS}"
  )
endfunction ()

function (quick_lint_js_add_library TARGET)
  add_library(${ARGV})
  target_compile_options(
    "${TARGET}"
    PRIVATE
    "${QUICK_LINT_JS_CXX_COMPILER_OPTIONS}"
  )
endfunction ()
