# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

set(QUICK_LINT_JS_CXX_COMPILER_OPTIONS "" CACHE STRING "")
mark_as_advanced(QUICK_LINT_JS_CXX_COMPILER_OPTIONS)

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
  if ("${TARGET}" STREQUAL quick-lint-js-precompiled-headers)
    # Don't use PCH when building PCH.
  else ()
    quick_lint_js_use_default_precompiled_headers("${TARGET}")
  endif ()
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
