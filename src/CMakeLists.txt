# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

cmake_minimum_required(VERSION 3.10)
include(GNUInstallDirs)
include(QuickLintJSCompiler)
include(QuickLintJSCopyright)
include(QuickLintJSTarget)

option(
  QUICK_LINT_JS_FEATURE_VECTOR_PROFILING
  "Enable the QLJS_DUMP_VECTORS option at run-time"
  FALSE
)

set(NEED_THREADS TRUE)

if (NEED_THREADS)
  find_package(Threads REQUIRED)
endif ()

set(EXTRA_SOURCES)
if (WIN32)
  list(APPEND EXTRA_SOURCES quick-lint-js.rc)
  # TODO(strager): Add a dependency on dusty-icon.ico which is referenced by
  # quick-lint-js.rc.
endif ()
quick_lint_js_add_executable(
  quick-lint-js
  main.cpp
  ${EXTRA_SOURCES}
)
set_target_properties(quick-lint-js PROPERTIES RUNTIME_OUTPUT_DIRECTORY ..)
target_link_libraries(quick-lint-js PRIVATE quick-lint-js-lib)
quick_lint_js_enable_windows_unicode(quick-lint-js)

# TODO(strager): Use the default DESTINATION for CMake versions which have a
# default. (3.18.0 has a default; 3.12.4 doesn't have a default.)
install(TARGETS quick-lint-js RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")

quick_lint_js_add_library(
  quick-lint-js-lib
  arg-parser.cpp
  assert.cpp
  basic-configuration-filesystem.cpp
  boost.cpp
  buffering-diag-reporter.cpp
  byte-buffer.cpp
  c-api-diag-reporter.cpp
  c-api.cpp
  change-detecting-filesystem-inotify.cpp
  change-detecting-filesystem-kqueue.cpp
  change-detecting-filesystem-win32.cpp
  change-detecting-filesystem.cpp
  char8.cpp
  cli-location.cpp
  configuration-loader.cpp
  configuration.cpp
  diagnostic-formatter.cpp
  diagnostic.cpp
  document.cpp
  emacs-lisp-diag-reporter.cpp
  emacs-location.cpp
  error-list.cpp
  file-canonical.cpp
  file-handle.cpp
  file-path.cpp
  file.cpp
  global-variables-browser.cpp
  global-variables.cpp
  integer.cpp
  json.cpp
  jsx.cpp
  lex-keyword.cpp
  lex-unicode.cpp
  lex.cpp
  lint.cpp
  locale.cpp
  location.cpp
  logger.cpp
  lsp-diag-reporter.cpp
  lsp-location.cpp
  lsp-message-parser.cpp
  lsp-pipe-writer.cpp
  lsp-server.cpp
  options.cpp
  output-stream.cpp
  padded-string.cpp
  parse.cpp
  pipe-writer.cpp
  pipe.cpp
  quick-lint-js/allocator.h
  quick-lint-js/arg-parser.h
  quick-lint-js/assert.h
  quick-lint-js/basic-configuration-filesystem.h
  quick-lint-js/bit.h
  quick-lint-js/buffering-diag-reporter.h
  quick-lint-js/buffering-visitor.h
  quick-lint-js/byte-buffer.h
  quick-lint-js/c-api-diag-reporter.h
  quick-lint-js/c-api.h
  quick-lint-js/change-detecting-filesystem.h
  quick-lint-js/char8.h
  quick-lint-js/cli-location.h
  quick-lint-js/configuration-loader.h
  quick-lint-js/configuration.h
  quick-lint-js/consteval.h
  quick-lint-js/cpp.h
  quick-lint-js/crash.h
  quick-lint-js/diag-reporter.h
  quick-lint-js/diagnostic-formatter.h
  quick-lint-js/diagnostic-types.h
  quick-lint-js/diagnostic.h
  quick-lint-js/document.h
  quick-lint-js/emacs-lisp-diag-reporter.h
  quick-lint-js/emacs-location.h
  quick-lint-js/error-list.h
  quick-lint-js/event-loop.h
  quick-lint-js/expression.h
  quick-lint-js/feature.h
  quick-lint-js/file-canonical.h
  quick-lint-js/file-handle.h
  quick-lint-js/file-path.h
  quick-lint-js/file.h
  quick-lint-js/force-inline.h
  quick-lint-js/global-variables.h
  quick-lint-js/hash-fnv.h
  quick-lint-js/have.h
  quick-lint-js/integer.h
  quick-lint-js/json.h
  quick-lint-js/jsx.h
  quick-lint-js/language.h
  quick-lint-js/lex.h
  quick-lint-js/linked-bump-allocator.h
  quick-lint-js/lint.h
  quick-lint-js/locale.h
  quick-lint-js/location.h
  quick-lint-js/log.h
  quick-lint-js/logger.h
  quick-lint-js/lsp-diag-reporter.h
  quick-lint-js/lsp-endpoint.h
  quick-lint-js/lsp-location.h
  quick-lint-js/lsp-message-parser.h
  quick-lint-js/lsp-pipe-writer.h
  quick-lint-js/lsp-server.h
  quick-lint-js/math-overflow.h
  quick-lint-js/narrow-cast.h
  quick-lint-js/null-visitor.h
  quick-lint-js/optional.h
  quick-lint-js/options.h
  quick-lint-js/output-stream.h
  quick-lint-js/padded-string.h
  quick-lint-js/parse-expression-inl.h
  quick-lint-js/parse-statement-inl.h
  quick-lint-js/parse-visitor.h
  quick-lint-js/parse.h
  quick-lint-js/pipe-writer.h
  quick-lint-js/pipe.h
  quick-lint-js/reported-diag-statistics.h
  quick-lint-js/result.h
  quick-lint-js/simd.h
  quick-lint-js/simdjson.h
  quick-lint-js/temporary-directory.h
  quick-lint-js/text-diag-reporter.h
  quick-lint-js/thread.h
  quick-lint-js/translation-table-generated.h
  quick-lint-js/translation-table.h
  quick-lint-js/translation.h
  quick-lint-js/unreachable.h
  quick-lint-js/uri.h
  quick-lint-js/utf-16.h
  quick-lint-js/utf-8.h
  quick-lint-js/vector.h
  quick-lint-js/version.h
  quick-lint-js/vim-location.h
  quick-lint-js/vim-qflist-json-diag-reporter.h
  quick-lint-js/warning.h
  quick-lint-js/web-demo-location.h
  quick-lint-js/windows.h
  simdjson.cpp
  temporary-directory.cpp
  text-diag-reporter.cpp
  thread.cpp
  translation-table-generated.cpp
  translation.cpp
  uri.cpp
  utf-16.cpp
  utf-8.cpp
  vim-location.cpp
  vim-qflist-json-diag-reporter.cpp
  web-demo-location.cpp
)
target_compile_definitions(
  quick-lint-js-lib
  PUBLIC
  "QUICK_LINT_JS_VERSION=${QUICK_LINT_JS_VERSION}"
)
target_include_directories(quick-lint-js-lib PUBLIC .)
target_link_libraries(quick-lint-js-lib PUBLIC boost simdjson)
if (WIN32)
  target_link_libraries(quick-lint-js-lib PUBLIC pathcch)
endif ()
if (NEED_THREADS)
  target_link_libraries(quick-lint-js-lib PRIVATE Threads::Threads)
endif ()
if (QUICK_LINT_JS_PRECOMPILE_HEADERS)
  target_precompile_headers(
    quick-lint-js-lib
    PUBLIC
    <boost/container/pmr/polymorphic_allocator.hpp>
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
if (QUICK_LINT_JS_FEATURE_VECTOR_PROFILING)
  target_link_libraries(quick-lint-js-lib PRIVATE quick-lint-js-lib-debug)
endif ()

quick_lint_js_have_charconv(QUICK_LINT_JS_HAVE_CHARCONV)
if (NOT QUICK_LINT_JS_HAVE_CHARCONV)
  target_compile_definitions(
    quick-lint-js-lib
    PUBLIC
    QLJS_HAVE_CHARCONV_HEADER=0
  )
endif ()

# quick-lint-js-lib-debug should be dead-code stripped from production builds.
# It is a separate library from quick-lint-js-lib to increase the chance that
# the linker drops the code entirely.
#
# Code in quick-lint-js-lib-debug can be used from tests, but should not be used
# in production code.
quick_lint_js_add_library(
  quick-lint-js-lib-debug
  char8-debug.cpp
  cli-location-debug.cpp
  emacs-location-debug.cpp
  error-debug.cpp
  language-debug.cpp
  lex-debug.cpp
  lsp-location-debug.cpp
  padded-string-debug.cpp
  vector-profiler-debug.cpp
  vim-location-debug.cpp
)
target_link_libraries(quick-lint-js-lib-debug PUBLIC boost quick-lint-js-lib)

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

quick_lint_js_collect_copyright(
  quick-lint-js-licenses
  TARGET quick-lint-js
  OUTPUT_FILE "${CMAKE_CURRENT_BINARY_DIR}/quick-lint-js-licenses.txt"
)
if (TARGET quick-lint-js-licenses)
  install(
    FILES "${CMAKE_CURRENT_BINARY_DIR}/quick-lint-js-licenses.txt"
    DESTINATION "${QUICK_LINT_JS_INSTALL_LICENSES_DIR}"
    RENAME copyright
  )
endif ()

quick_lint_js_add_library(
  quick-lint-js-boost-json
  boost-json.cpp
  quick-lint-js/boost-json.h
)
target_include_directories(quick-lint-js-boost-json PUBLIC .)
target_link_libraries(
  quick-lint-js-boost-json
  PUBLIC
  boost_json
  quick-lint-js-lib
)
target_compile_definitions(
  quick-lint-js-boost-json
  PRIVATE
  QLJS_BUILDING_QLJS_BOOST_JSON
)

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
