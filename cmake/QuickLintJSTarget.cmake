# Copyright (C) 2020  Matthew Glazar
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
endfunction ()

function (quick_lint_js_add_library TARGET)
  add_library(${ARGV})
  target_compile_options(
    "${TARGET}"
    PRIVATE
    "${QUICK_LINT_JS_CXX_COMPILER_OPTIONS}"
  )
endfunction ()

# quick-lint-js finds bugs in JavaScript programs.
# Copyright (C) 2020  Matthew Glazar
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
