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

  list(APPEND CMAKE_REQUIRED_LIBRARIES stdc++fs)
  check_cxx_source_compiles(
    "${STD_FILESYSTEM_SOURCE}"
    QUICK_LINT_JS_HAVE_STD_FILESYSTEM_WITH_STDCXXFS
  )
  check_cxx_source_compiles(
    "${STD_EXPERIMENTAL_FILESYSTEM_SOURCE}"
    QUICK_LINT_JS_HAVE_STD_EXPERIMENTAL_FILESYSTEM_WITH_STDCXXFS
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
  if (
    QUICK_LINT_JS_HAVE_STD_FILESYSTEM
    OR QUICK_LINT_JS_HAVE_STD_EXPERIMENTAL_FILESYSTEM
  )
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
