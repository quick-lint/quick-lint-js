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
  parse-json.cpp
  pipe.cpp
  spy-visitor.cpp
  test-assert.cpp
  test-bit.cpp
  test-buffering-visitor.cpp
  test-constant-divider.cpp
  test-crash.cpp
  test-file.cpp
  test-gmo.cpp
  test-integer-decimal.cpp
  test-integer-hexadecimal.cpp
  test-lex.cpp
  test-lint-parse.cpp
  test-lint.cpp
  test-locale.cpp
  test-location.cpp
  test-lsp-endpoint.cpp
  test-lsp-error-reporter.cpp
  test-lsp-message-parser.cpp
  test-lsp-pipe-reader.cpp
  test-lsp-pipe-writer.cpp
  test-math-overflow.cpp
  test-narrow-cast.cpp
  test-options.cpp
  test-padded-string.cpp
  test-parse-expression.cpp
  test-parse.cpp
  test-text-error-reporter.cpp
  test-translation.cpp
  test-vector.cpp
  test-vim-qflist-json-error-reporter.cpp
  test-wasm-demo-error-reporter.cpp
  test-write-integer.cpp
)
target_include_directories(quick-lint-js-test PRIVATE .)
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

# HACK(strager): UndefinedBehaviorSanitizer is buggy on Linunx x86_64 with GCC
# 8.4.0. Disable some UBSAN checks to avoid false positives.
if (
  CMAKE_CXX_COMPILER_ID STREQUAL GNU
  AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS 9.0.0
)
  set_source_files_properties(
    test-lex.cpp
    PROPERTIES
    COMPILE_OPTIONS -fno-sanitize=null
  )
endif ()
