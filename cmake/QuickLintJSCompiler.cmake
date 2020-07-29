include(CheckCXXSourceCompiles)

function (quick_lint_js_check_finally OUT_EXTRA_COMPILE_OPTIONS)
  check_cxx_source_compiles(
    "int main() { int _finally = 0; return _finally; }"
    QUICK_LINT_JS_COMPILER_ALLOWS_VARIABLE_NAMED_FINALLY
  )

  if (QUICK_LINT_JS_COMPILER_ALLOWS_VARIABLE_NAMED_FINALLY)
    set(QUICK_LINT_JS_COMPILER_ALLOWS_VARIABLE_NAMED_FINALLY_WITH_ZA FALSE CACHE INTERNAL "")
    set("${OUT_EXTRA_COMPILE_OPTIONS}" "" PARENT_SCOPE)
  else ()
    set(CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS} /Za")
    check_cxx_source_compiles(
      "int main() { int _finally = 0; return _finally; }"
      QUICK_LINT_JS_COMPILER_ALLOWS_VARIABLE_NAMED_FINALLY_WITH_ZA
    )
    set("${OUT_EXTRA_COMPILE_OPTIONS}" "/Za" PARENT_SCOPE)
  endif ()
endfunction ()

function (quick_lint_js_check_designated_initializers OUT)
  check_cxx_source_compiles(
    "struct s { int m; }; int main() { s x{.m = 0}; return x.m; }"
    "${OUT}"
  )
endfunction ()

function (quick_lint_js_set_cxx_standard)
  set(CMAKE_CXX_STANDARD_REQUIRED TRUE)
  if (cxx_std_20 IN_LIST CMAKE_CXX_COMPILE_FEATURES)
    set(CMAKE_CXX_STANDARD 20)
  else ()
    set(CMAKE_CXX_STANDARD 17)
    quick_lint_js_check_designated_initializers(
      QUICK_LINT_JS_COMPILER_SUPPORTS_DESIGNATED_INITIALIZERS
    )
    if (NOT "${QUICK_LINT_JS_COMPILER_SUPPORTS_DESIGNATED_INITIALIZERS}")
      message(
        FATAL_ERROR
        "C++ compiler does not support designated initializers (either GNU extension or C++20)"
      )
    endif ()
  endif ()

  set(CMAKE_CXX_STANDARD "${CMAKE_CXX_STANDARD}" PARENT_SCOPE)
  set(CMAKE_CXX_STANDARD_REQUIRED "${CMAKE_CXX_STANDARD_REQUIRED}" PARENT_SCOPE)
endfunction ()
