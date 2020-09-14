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
  quick-lint-js-test
  error-collector.cpp
  error-matcher.cpp
  spy-visitor.cpp
  test-assert.cpp
  test-buffering-visitor.cpp
  test-file.cpp
  test-integer.cpp
  test-lex.cpp
  test-lint-parse.cpp
  test-lint.cpp
  test-location.cpp
  test-narrow-cast.cpp
  test-options.cpp
  test-padded-string.cpp
  test-parse-expression.cpp
  test-parse.cpp
  test-text-error-reporter.cpp
  test-vector.cpp
  test-vim-qflist-json-error-reporter.cpp
)
target_include_directories(quick-lint-js-test PRIVATE include)
target_link_libraries(
  quick-lint-js-test
  PRIVATE
  gmock
  gmock_main
  gtest
  jsoncpp_lib
  quick-lint-js-lib
)
quick_lint_js_use_cxx_filesystem(quick-lint-js-test PRIVATE)
if (${CMAKE_VERSION} VERSION_GREATER_EQUAL 3.17.3)
  target_precompile_headers(
    quick-lint-js-test
    PRIVATE
    <gmock/gmock.h>
    <gtest/gtest.h>
    <iostream>
    <json/reader.h>
    <json/value.h>
    <json/writer.h>
  )
endif ()

add_test(
  NAME quick-lint-js-test
  COMMAND quick-lint-js-test
)

# Disable MSVC warnings in googletest's headers.
# TODO(strager): Use MSVC's /external switches instead.
quick_lint_js_add_warning_options_if_supported(
  quick-lint-js-test
  PRIVATE
  /wd26495 # Always initialize a member variable
  /wd26812 # Prefer 'enum class' over 'enum'
)
