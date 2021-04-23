# Copyright (C) 2020  Matthew Glazar
# See end of file for extended copyright information.

cmake_minimum_required(VERSION 3.10)
include(QuickLintJSCompiler)
include(QuickLintJSTarget)

quick_lint_js_add_executable(
  quick-lint-js-test
  error-collector.cpp
  error-matcher.cpp
  parse-json.cpp
  parse-support.cpp
  pipe.cpp
  quick-lint-js/array.h
  quick-lint-js/characters.h
  quick-lint-js/error-collector.h
  quick-lint-js/error-matcher.h
  quick-lint-js/gtest.h
  quick-lint-js/parse-json.h
  quick-lint-js/parse-support.h
  quick-lint-js/pipe.h
  quick-lint-js/spy-lsp-endpoint-remote.h
  quick-lint-js/spy-lsp-message-parser.h
  quick-lint-js/spy-visitor.h
  spy-visitor.cpp
  test-array.cpp
  test-assert.cpp
  test-bit.cpp
  test-buffering-error-reporter.cpp
  test-buffering-visitor.cpp
  test-byte-buffer.cpp
  test-cli-location.cpp
  test-constant-divider.cpp
  test-crash.cpp
  test-error-formatter.cpp
  test-error-list.cpp
  test-error.cpp
  test-file.cpp
  test-gmo.cpp
  test-integer-decimal.cpp
  test-integer-hexadecimal.cpp
  test-json.cpp
  test-lex.cpp
  test-lint-parse.cpp
  test-lint.cpp
  test-locale.cpp
  test-lsp-document.cpp
  test-lsp-endpoint.cpp
  test-lsp-error-reporter.cpp
  test-lsp-location.cpp
  test-lsp-message-parser.cpp
  test-lsp-pipe-reader.cpp
  test-lsp-pipe-writer.cpp
  test-lsp-server.cpp
  test-math-overflow.cpp
  test-monotonic-allocator.cpp
  test-narrow-cast.cpp
  test-options.cpp
  test-padded-string.cpp
  test-parse-class.cpp
  test-parse-expression-statement.cpp
  test-parse-expression.cpp
  test-parse-function.cpp
  test-parse-loop.cpp
  test-parse-module.cpp
  test-parse-statement.cpp
  test-parse-var.cpp
  test-parse.cpp
  test-text-error-reporter.cpp
  test-translation.cpp
  test-utf-16.cpp
  test-utf-8.cpp
  test-vector.cpp
  test-vim-location.cpp
  test-vim-qflist-json-error-reporter.cpp
  test-vscode-error-reporter.cpp
  test-vscode.cpp
  test-wasm-demo-error-reporter.cpp
  test-wasm-demo-location.cpp
  test-write-integer.cpp
)
target_include_directories(quick-lint-js-test PRIVATE .)
target_link_libraries(
  quick-lint-js-test
  PRIVATE
  gmock
  gmock_main
  gtest
  jsoncpp_static
  quick-lint-js-lib
)
if (${CMAKE_VERSION} VERSION_GREATER_EQUAL 3.17.3)
  target_precompile_headers(
    quick-lint-js-test
    PRIVATE
    <boost/container/pmr/monotonic_buffer_resource.hpp>
    <boost/container/pmr/polymorphic_allocator.hpp>
    <boost/container/small_vector.hpp>
    <gmock/gmock.h>
    <gtest/gtest.h>
    <iostream>
    <json/reader.h>
    <json/value.h>
    <json/writer.h>
    <simdjson.h>
  )
endif ()
quick_lint_js_enable_bigobj_if_supported(quick-lint-js-test)

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

# HACK(strager): UndefinedBehaviorSanitizer is buggy on Linux x86_64 with GCC
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

find_package(ICU COMPONENTS uc)
if (ICU_FOUND)
  quick_lint_js_add_executable(
    quick-lint-js-test-lex-unicode
    test-lex-unicode.cpp
  )
  target_link_libraries(
    quick-lint-js-test-lex-unicode
    PRIVATE
    ICU::uc
    gmock
    gmock_main
    gtest
    quick-lint-js-lib
  )
  if (${CMAKE_VERSION} VERSION_GREATER_EQUAL 3.17.3)
    target_precompile_headers(
      quick-lint-js-test-lex-unicode
      PRIVATE
      <gmock/gmock.h>
      <gtest/gtest.h>
    )
  endif ()

  add_test(
    NAME quick-lint-js-test-lex-unicode
    COMMAND quick-lint-js-test-lex-unicode
  )
endif ()

find_package(PythonInterp 3.7)
if (PYTHONINTERP_FOUND)
  add_test(
    NAME quick-lint-js-test-cli
    COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test-cli.py
  )
  set_property(
    TEST quick-lint-js-test-cli
    APPEND PROPERTY ENVIRONMENT QUICK_LINT_JS_EXE=$<TARGET_FILE:quick-lint-js>
  )
endif ()

# quick-lint-js finds bugs in JavaScript programs.
# Copyright (C) 2020  Matthew Glazar
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
