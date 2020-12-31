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
include(GNUInstallDirs)
include(QuickLintJSCompiler)
include(QuickLintJSTarget)

option(
  QUICK_LINT_JS_FEATURE_VECTOR_PROFILING
  "Enable the QLJS_DUMP_VECTORS option at run-time"
  FALSE
)

quick_lint_js_add_executable(
  quick-lint-js
  main.cpp
)
set_target_properties(quick-lint-js PROPERTIES RUNTIME_OUTPUT_DIRECTORY ..)
target_link_libraries(quick-lint-js PRIVATE quick-lint-js-lib)

# TODO(strager): Use the default DESTINATION for CMake versions which have a
# default. (3.18.0 has a default; 3.12.4 doesn't have a default.)
install(TARGETS quick-lint-js RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")

quick_lint_js_add_library(
  quick-lint-js-lib
  assert.cpp
  byte-buffer.cpp
  char8.cpp
  cli-location.cpp
  crash.cpp
  error.cpp
  file-handle.cpp
  file.cpp
  gmo.cpp
  integer.cpp
  json.cpp
  language.cpp
  lex-keyword.cpp
  lex-unicode.cpp
  lex.cpp
  lint.cpp
  locale.cpp
  location.cpp
  lsp-document.cpp
  lsp-error-reporter.cpp
  lsp-location.cpp
  lsp-message-parser.cpp
  lsp-pipe-writer.cpp
  lsp-server.cpp
  options.cpp
  padded-string.cpp
  parse.cpp
  quick-exit.cpp
  quick-lint-js/assert.h
  quick-lint-js/bit.h
  quick-lint-js/buffering-visitor.h
  quick-lint-js/byte-buffer.h
  quick-lint-js/char8.h
  quick-lint-js/cli-location.h
  quick-lint-js/constant-divider.h
  quick-lint-js/cpp.h
  quick-lint-js/crash.h
  quick-lint-js/error-formatter.h
  quick-lint-js/error-tape.h
  quick-lint-js/error.h
  quick-lint-js/expression.h
  quick-lint-js/feature.h
  quick-lint-js/file-handle.h
  quick-lint-js/file.h
  quick-lint-js/force-inline.h
  quick-lint-js/gmo.h
  quick-lint-js/have.h
  quick-lint-js/integer.h
  quick-lint-js/json.h
  quick-lint-js/language.h
  quick-lint-js/lex.h
  quick-lint-js/lint.h
  quick-lint-js/locale.h
  quick-lint-js/location.h
  quick-lint-js/lsp-document.h
  quick-lint-js/lsp-endpoint.h
  quick-lint-js/lsp-error-reporter.h
  quick-lint-js/lsp-location.h
  quick-lint-js/lsp-message-parser.h
  quick-lint-js/lsp-pipe-writer.h
  quick-lint-js/lsp-server.h
  quick-lint-js/math-overflow.h
  quick-lint-js/narrow-cast.h
  quick-lint-js/null-visitor.h
  quick-lint-js/optional.h
  quick-lint-js/options.h
  quick-lint-js/padded-string.h
  quick-lint-js/parse-visitor.h
  quick-lint-js/parse.h
  quick-lint-js/pipe-reader.h
  quick-lint-js/quick-exit.h
  quick-lint-js/simd.h
  quick-lint-js/std-filesystem.h
  quick-lint-js/text-error-reporter.h
  quick-lint-js/translation-data.h
  quick-lint-js/translation.h
  quick-lint-js/unreachable.h
  quick-lint-js/utf-16.h
  quick-lint-js/utf-8.h
  quick-lint-js/vector.h
  quick-lint-js/version.h
  quick-lint-js/vim-location.h
  quick-lint-js/vim-qflist-json-error-reporter.h
  quick-lint-js/vscode-error-reporter.h
  quick-lint-js/vscode.h
  quick-lint-js/warning.h
  quick-lint-js/wasm-demo-error-reporter.h
  quick-lint-js/wasm-demo-location.h
  text-error-reporter.cpp
  translation-data.cpp
  translation.cpp
  utf-16.cpp
  utf-8.cpp
  vector.cpp
  vim-location.cpp
  vim-qflist-json-error-reporter.cpp
  vscode-error-reporter.cpp
  vscode.cpp
  wasm-demo-error-reporter.cpp
  wasm-demo-location.cpp
)
target_include_directories(quick-lint-js-lib PUBLIC .)
target_link_libraries(quick-lint-js-lib PUBLIC boost_container simdjson)
if (${CMAKE_VERSION} VERSION_GREATER_EQUAL 3.17.3)
  target_precompile_headers(
    quick-lint-js-lib
    PUBLIC
    <boost/container/pmr/polymorphic_allocator.hpp>
    <boost/container/small_vector.hpp>
    <cmath>
    <cstring>
    <simdjson.h>
    <string>
  )
endif ()

if (QUICK_LINT_JS_FEATURE_VECTOR_PROFILING)
  target_compile_definitions(
    quick-lint-js-lib
    PUBLIC
    QLJS_FEATURE_VECTOR_PROFILING=1
  )
else ()
  target_compile_definitions(
    quick-lint-js-lib
    PUBLIC
    QLJS_FEATURE_VECTOR_PROFILING=0
  )
endif ()

# HACK(strager): Work around GCC compiler bug. GCC 9.3.0 miscompiles a call to
# strncmp, causing the length given to strncmp to be incorrect. (Perhaps we are
# invoking undefined behaviour though with our string table offset pointer
# arithmetic? Unsure.) The bug appears to be fixed in GCC 10.1.0.
#
# Possibly related: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=92765
#
# To work around the bug, apply -fno-builtin to the target. If we apply it only
# to the source file, the flag seems to be ignored when using precompiled
# headers.
if (
  CMAKE_CXX_COMPILER_ID STREQUAL GNU
  AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS 10.1.0
)
  target_compile_options(quick-lint-js-lib PRIVATE -fno-builtin)
endif ()
