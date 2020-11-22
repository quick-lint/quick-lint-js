include(CheckCXXCompilerFlag)
include(CheckCXXSourceCompiles)

function (quick_lint_js_check_designated_initializers OUT)
  check_cxx_source_compiles(
    "struct s { int m; }; int main() { s x{.m = 0}; return x.m; }"
    "${OUT}"
  )
endfunction ()

function (quick_lint_js_enable_char8_t_if_supported)
  check_cxx_compiler_flag(-fchar8_t QUICK_LINT_JS_HAVE_FCHAR8_T)
  if (QUICK_LINT_JS_HAVE_FCHAR8_T)
    add_compile_options($<$<COMPILE_LANGUAGE:CXX>:-fchar8_t>)
  endif ()
endfunction ()

function (quick_lint_js_configure_exception_handling)
  if (MSVC)
    add_compile_options(/EHcs)
  endif ()
endfunction ()

function (quick_lint_js_use_cxx_filesystem TARGET VISIBILITY)
  quick_lint_js_check_cxx_filesystem(
    REQUIRED_LIBRARIES stdc++fs
    OUT_VAR_SUFFIX _WITH_STDCXXFS
  )
  if (
    QUICK_LINT_JS_HAVE_STD_FILESYSTEM_WITH_STDCXXFS
    OR QUICK_LINT_JS_HAVE_STD_EXPERIMENTAL_FILESYSTEM_WITH_STDCXXFS
  )
    # On some systems, linking to stdc++fs is required even if complication
    # succeeds without stdc++fs. Be conservative and link to stdc++fs if it
    # exists.
    # Example: https://stackoverflow.com/questions/56615841/passing-stdfilesystempath-to-a-function-segfaults
    target_link_libraries("${TARGET}" "${VISIBILITY}" stdc++fs)
    return ()
  endif ()

  quick_lint_js_check_cxx_filesystem(OUT_VAR_SUFFIX "")
  if (
    QUICK_LINT_JS_HAVE_STD_FILESYSTEM
    OR QUICK_LINT_JS_HAVE_STD_EXPERIMENTAL_FILESYSTEM
  )
    return ()
  endif ()

  quick_lint_js_check_cxx_filesystem(
    REQUIRED_LIBRARIES c++fs
    OUT_VAR_SUFFIX _WITH_CXXFS
  )
  if (
      QUICK_LINT_JS_HAVE_STD_FILESYSTEM_WITH_CXXFS
      OR QUICK_LINT_JS_HAVE_STD_EXPERIMENTAL_FILESYSTEM_WITH_CXXFS
  )
    target_link_libraries("${TARGET}" "${VISIBILITY}" c++fs)
    return ()
  endif ()

  quick_lint_js_check_cxx_filesystem(
    REQUIRED_LIBRARIES c++experimental
    OUT_VAR_SUFFIX _WITH_CXXEXPERIMENTAL
  )
  if (
      QUICK_LINT_JS_HAVE_STD_FILESYSTEM_WITH_CXXEXPERIMENTAL
      OR QUICK_LINT_JS_HAVE_STD_EXPERIMENTAL_FILESYSTEM_WITH_CXXEXPERIMENTAL
  )
    target_link_libraries("${TARGET}" "${VISIBILITY}" c++experimental)
    return ()
  endif ()

  message(FATAL_ERROR "C++ compiler does not support C++17 std::filesystem")
endfunction ()

function (quick_lint_js_check_cxx_filesystem)
  cmake_parse_arguments("" "" "OUT_VAR_SUFFIX" "REQUIRED_LIBRARIES" ${ARGN})

  set(STD_FILESYSTEM_SOURCE "#include <filesystem>\nint main() { return ::std::filesystem::temp_directory_path().is_absolute(); }")
  set(STD_EXPERIMENTAL_FILESYSTEM_SOURCE "#include <experimental/filesystem>\nint main() { return ::std::experimental::filesystem::temp_directory_path().is_absolute(); }")

  list(APPEND CMAKE_REQUIRED_LIBRARIES ${_REQUIRED_LIBRARIES})
  check_cxx_source_compiles(
    "${STD_FILESYSTEM_SOURCE}"
    "QUICK_LINT_JS_HAVE_STD_FILESYSTEM${_OUT_VAR_SUFFIX}"
  )
  check_cxx_source_compiles(
    "${STD_EXPERIMENTAL_FILESYSTEM_SOURCE}"
    "QUICK_LINT_JS_HAVE_STD_EXPERIMENTAL_FILESYSTEM${_OUT_VAR_SUFFIX}"
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

function (quick_lint_js_add_warning_options_if_supported)
  cmake_parse_arguments("" "" "" "INTERFACE;PRIVATE;PUBLIC" ${ARGN})
  set(TARGETS "${_UNPARSED_ARGUMENTS}")

  foreach (STYLE INTERFACE PRIVATE PUBLIC)
    quick_lint_js_get_supported_warning_options(
      "${_${STYLE}}"
      WARNING_OPTIONS_TO_ADD
    )
    if (WARNING_OPTIONS_TO_ADD)
      target_compile_options("${TARGETS}" "${STYLE}" "${WARNING_OPTIONS_TO_ADD}")
    endif ()
  endforeach ()
endfunction ()

function (quick_lint_js_get_supported_warning_options WARNING_OPTIONS OUT_SUPPORTED_WARNING_OPTIONS)
  set(SUPPORTED_WARNING_OPTIONS)
  foreach (WARNING_OPTION IN LISTS WARNING_OPTIONS)
    quick_lint_js_warning_option_var_name("${WARNING_OPTION}" WARNING_OPTION_VAR_NAME)
    check_cxx_compiler_flag("${WARNING_OPTION}" "${WARNING_OPTION_VAR_NAME}")
    if ("${${WARNING_OPTION_VAR_NAME}}")
      list(APPEND SUPPORTED_WARNING_OPTIONS "${WARNING_OPTION}")
    endif ()
  endforeach ()
  set("${OUT_SUPPORTED_WARNING_OPTIONS}" "${SUPPORTED_WARNING_OPTIONS}" PARENT_SCOPE)
endfunction ()

function (quick_lint_js_warning_option_var_name WARNING_OPTION OUT_VAR_NAME)
  set(VAR_NAME "${WARNING_OPTION}")
  string(TOUPPER "${VAR_NAME}" VAR_NAME)
  string(REGEX REPLACE "[-/]" _ VAR_NAME "${VAR_NAME}")
  set(VAR_NAME "QUICK_LINT_JS_HAVE_WARNING_OPTION_${VAR_NAME}")
  set("${OUT_VAR_NAME}" "${VAR_NAME}" PARENT_SCOPE)
endfunction ()

function (quick_lint_js_work_around_implicit_link_directories)
  # HACK(strager): Work around weird CMake behaviors when mixing multiple
  # languages (e.g. C and C++).
  #
  # Let's say the user asks CMake to compile C code with compiler X and C++ code
  # with compiler Y. Let's say a CMake file says we need to link an executable
  # which mixes C code and C++ code. CMake does the following:
  #
  # 1. Ask compiler X for its implicit linker flags.
  # 2. Use compiler Y to link the executable. Use some of compiler X's implicit
  #    linker flags.
  #
  # This is a problem when using compiler X's flags break compiler Y in some
  # way. In the wild, we observed that, if compiler X is GCC 7 and compiler Y is
  # GCC 8, CMake links the executable with GCC 8 but uses GCC 7's path to
  # libstdc++. This means that sources are compiled with GCC 8's libstdc++ but
  # link with GCC 7's libstdc++.
  #
  # Work around this by having compiler Y's flags take priority over compiler
  # X's flags. We need to clear CMAKE_<LANG>_IMPLICIT_LINK_DIRECTORIES;
  # otherwise, CMake ignores the link_directories command because it thinks the
  # compiler will already include those directories.
  #
  # See this issue for more details:
  # https://github.com/quick-lint/quick-lint-js/issues/9
  link_directories(${CMAKE_CXX_IMPLICIT_LINK_DIRECTORIES})
  set(CMAKE_CXX_IMPLICIT_LINK_DIRECTORIES "" PARENT_SCOPE)
endfunction ()
