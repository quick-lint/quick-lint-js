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

cmake_minimum_required(VERSION 3.10)
include(QuickLintJSCompiler)
include(QuickLintJSTarget)

quick_lint_js_add_executable(
  quick-lint-js
  main.cpp
)
target_link_libraries(quick-lint-js PRIVATE quick-lint-js-lib)

quick_lint_js_add_library(
  quick-lint-js-lib
  file.cpp
  integer.cpp
  lex-keyword.cpp
  lex.cpp
  lint.cpp
  location.cpp
  options.cpp
  padded-string.cpp
  parse.cpp
  text-error-reporter.cpp
  vim-qflist-json-error-reporter.cpp
)
target_include_directories(quick-lint-js-lib PUBLIC include)
target_link_libraries(
  quick-lint-js-lib
  PUBLIC
  boost_container
  PRIVATE
  gnulib-getopt
)

# HACK(strager): Work around GCC compiler bug. GCC 9.3.0 miscompiles a call to
# strncmp, causing the length given to strncmp to be incorrect. (Perhaps we are
# invoking undefined behaviour though with our string table offset pointer
# arithmetic? Unsure.) The bug appears to be fixed in GCC 10.1.0.
# Possibly related: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=92765
if (
  CMAKE_CXX_COMPILER_ID STREQUAL GNU
  AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS 10.1.0
)
  set_source_files_properties(
    lex-keyword.cpp
    PROPERTIES
    COMPILE_OPTIONS -fno-builtin
  )
endif ()
