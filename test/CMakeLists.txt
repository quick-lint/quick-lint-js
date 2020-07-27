# quicklint-js finds bugs in JavaScript programs.
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

add_executable(
  quicklint-js-test
  test-lex.cpp
  test-lint.cpp
  test-location.cpp
  test-narrow-cast.cpp
  test-options.cpp
  test-parse-expression.cpp
  test-parse.cpp
)
target_include_directories(quicklint-js-test PRIVATE include)
target_link_libraries(
  quicklint-js-test
  PRIVATE
  doctest
  doctest_with_main
  quicklint-js-lib
)

add_test(
  NAME quicklint-js-test
  COMMAND quicklint-js-test
)
