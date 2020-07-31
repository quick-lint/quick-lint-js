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

add_executable(
  quick-lint-js
  main.cpp
)
target_link_libraries(quick-lint-js PRIVATE quick-lint-js-lib)

add_library(
  quick-lint-js-lib
  lex.cpp
  lint.cpp
  location.cpp
  options.cpp
  parse.cpp
)
quick_lint_js_check_finally(EXTRA_COMPILE_OPTIONS)
target_compile_options(quick-lint-js-lib PUBLIC "${EXTRA_COMPILE_OPTIONS}")
target_include_directories(quick-lint-js-lib PUBLIC include)
target_link_libraries(quick-lint-js-lib PRIVATE gnulib-getopt)
