include(CheckCXXCompilerFlag)
include(CheckCXXSourceCompiles)

function (quick_lint_js_check_designated_initializers OUT)
  check_cxx_source_compiles(
    "struct s { int m; }; int main() { s x{.m = 0}; return x.m; }"
    "${OUT}"
  )
endfunction ()

function (quick_lint_js_use_cxx_filesystem TARGET VISIBILITY)
  set(STD_FILESYSTEM_SOURCE "#include <filesystem>\nint main() { return ::std::filesystem::temp_directory_path().is_absolute(); }")
  set(STD_EXPERIMENTAL_FILESYSTEM_SOURCE "#include <experimental/filesystem>\nint main() { return ::std::experimental::filesystem::temp_directory_path().is_absolute(); }")

  check_cxx_source_compiles(
    "${STD_FILESYSTEM_SOURCE}"
    QUICK_LINT_JS_HAVE_STD_FILESYSTEM
  )
  check_cxx_source_compiles(
    "${STD_EXPERIMENTAL_FILESYSTEM_SOURCE}"
    QUICK_LINT_JS_HAVE_STD_EXPERIMENTAL_FILESYSTEM
  )

  set(OLD_CMAKE_REQUIRED_LIBRARIES "${CMAKE_REQUIRED_LIBRARIES}")
  list(APPEND CMAKE_REQUIRED_LIBRARIES stdc++fs)
  check_cxx_source_compiles(
    "${STD_FILESYSTEM_SOURCE}"
    QUICK_LINT_JS_HAVE_STD_FILESYSTEM_WITH_STDCXXFS
  )
  check_cxx_source_compiles(
    "${STD_EXPERIMENTAL_FILESYSTEM_SOURCE}"
    QUICK_LINT_JS_HAVE_STD_EXPERIMENTAL_FILESYSTEM_WITH_STDCXXFS
  )
  set(CMAKE_REQUIRED_LIBRARIES "${OLD_CMAKE_REQUIRED_LIBRARIES}")

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
  if (
    QUICK_LINT_JS_HAVE_STD_FILESYSTEM
    OR QUICK_LINT_JS_HAVE_STD_EXPERIMENTAL_FILESYSTEM
  )
    return ()
  endif ()

  set(OLD_CMAKE_REQUIRED_LIBRARIES "${CMAKE_REQUIRED_LIBRARIES}")
  list(APPEND CMAKE_REQUIRED_LIBRARIES c++fs)
  check_cxx_source_compiles(
    "${STD_FILESYSTEM_SOURCE}"
    QUICK_LINT_JS_HAVE_STD_FILESYSTEM_WITH_CXXFS
  )
  check_cxx_source_compiles(
    "${STD_EXPERIMENTAL_FILESYSTEM_SOURCE}"
    QUICK_LINT_JS_HAVE_STD_EXPERIMENTAL_FILESYSTEM_WITH_CXXFS
  )
  set(CMAKE_REQUIRED_LIBRARIES "${OLD_CMAKE_REQUIRED_LIBRARIES}")
  if (
      QUICK_LINT_JS_HAVE_STD_FILESYSTEM_WITH_CXXFS
      OR QUICK_LINT_JS_HAVE_STD_EXPERIMENTAL_FILESYSTEM_WITH_CXXFS
  )
    target_link_libraries("${TARGET}" "${VISIBILITY}" c++fs)
    return ()
  endif ()

  set(OLD_CMAKE_REQUIRED_LIBRARIES "${CMAKE_REQUIRED_LIBRARIES}")
  list(APPEND CMAKE_REQUIRED_LIBRARIES c++experimental)
  check_cxx_source_compiles(
    "${STD_FILESYSTEM_SOURCE}"
    QUICK_LINT_JS_HAVE_STD_FILESYSTEM_WITH_CXXEXPERIMENTAL
  )
  check_cxx_source_compiles(
    "${STD_EXPERIMENTAL_FILESYSTEM_SOURCE}"
    QUICK_LINT_JS_HAVE_STD_EXPERIMENTAL_FILESYSTEM_WITH_CXXEXPERIMENTAL
  )
  set(CMAKE_REQUIRED_LIBRARIES "${OLD_CMAKE_REQUIRED_LIBRARIES}")
  if (
      QUICK_LINT_JS_HAVE_STD_FILESYSTEM_WITH_CXXEXPERIMENTAL
      OR QUICK_LINT_JS_HAVE_STD_EXPERIMENTAL_FILESYSTEM_WITH_CXXEXPERIMENTAL
  )
    target_link_libraries("${TARGET}" "${VISIBILITY}" c++experimental)
    return ()
  endif ()

  message(FATAL_ERROR "C++ compiler does not support C++17 std::filesystem")
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
