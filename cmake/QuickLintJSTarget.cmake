# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

set(QUICK_LINT_JS_CXX_COMPILER_OPTIONS "" CACHE STRING "")
mark_as_advanced(QUICK_LINT_JS_CXX_COMPILER_OPTIONS)

set(QUICK_LINT_JS_EMPTY_CPP "${CMAKE_CURRENT_LIST_DIR}/empty.cpp")

function (quick_lint_js_add_executable TARGET)
  add_executable(${ARGV})
  target_compile_options(
    "${TARGET}"
    PRIVATE
    "${QUICK_LINT_JS_CXX_COMPILER_OPTIONS}"
  )
  if ("${TARGET}" STREQUAL quick-lint-js-test OR
      "${TARGET}" STREQUAL quick-lint-js-test-lex-unicode)
    # HACK(strager): Tests have their own precompiled headers.
  else ()
    quick_lint_js_use_default_precompiled_headers("${TARGET}")
  endif ()
endfunction ()

function (quick_lint_js_add_library TARGET)
  add_library(${ARGV})
  target_compile_options(
    "${TARGET}"
    PRIVATE
    "${QUICK_LINT_JS_CXX_COMPILER_OPTIONS}"
  )
  quick_lint_js_use_default_precompiled_headers("${TARGET}")
endfunction ()

# Like add_library("${TARGET}") but with workarounds for bugs.
#
# * Avoid an INTERFACE library to work around the following error from CMake:
#   > Unable to resolve full path of PCH-header '[snip]/cmake_pch_arm64.hxx'
#   > assigned to target quick-lint-js-test-lib, although its path is supposed
#   > to be known!" errors from CMake.
# * Create a dummy source file because CMake demands one.
# * NOTE[empty-library-symbol]: Ensure there is at least one symbol to avoid the
#   following error from macOS's linker (as of Xcode 15.1, dyld version 1022.1):
#   > ld: archive member '/' not a mach-o file in
#   > '[snip]/libquick-lint-js-precompiled-headers.a'
function (quick_lint_js_add_empty_cxx_library TARGET)
  add_library("${TARGET}" STATIC "${QUICK_LINT_JS_EMPTY_CPP}")

  # NOTE(strager): Don't enable pre-compiled headers.
  # quick_lint_js_add_empty_cxx_library is used to define the main pre-compiled
  # headers CMake target and we don't want a dependency cycle.

  # NOTE(strager): We need QUICK_LINT_JS_CXX_COMPILER_OPTIONS. Without it, we
  # get miscompilations with some GCC flags (I don't know which) which make
  # pre-compiled headers incompatible.
  target_compile_options(
    "${TARGET}"
    PRIVATE
    "${QUICK_LINT_JS_CXX_COMPILER_OPTIONS}"
  )
endfunction ()

# Like add_library("${TARGET}") but with workarounds for bugs.
#
# See quick_lint_js_add_empty_cxx_library for details.
function (quick_lint_js_add_empty_executable TARGET)
  add_executable("${TARGET}" "${QUICK_LINT_JS_EMPTY_CPP}")
endfunction ()

function (quick_lint_js_use_default_precompiled_headers TARGET)
  if (QUICK_LINT_JS_PRECOMPILE_HEADERS)
    target_link_libraries("${TARGET}" PRIVATE quick-lint-js-precompiled-headers)
    target_precompile_headers(
      "${TARGET}"
      REUSE_FROM
      quick-lint-js-precompiled-headers
    )
  endif ()
endfunction ()

# quick-lint-js finds bugs in JavaScript programs.
# Copyright (C) 2020  Matthew "strager" Glazar
#
# This file is part of quick-lint-js.
#
# quick-lint-js is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# quick-lint-js is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with quick-lint-js.  If not, see <https://www.gnu.org/licenses/>.
