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
  basic-configuration-filesystem.cpp
  buffering-error-reporter.cpp
  byte-buffer.cpp
  c-api-error-reporter.cpp
  c-api.cpp
  change-detecting-filesystem-inotify.cpp
  change-detecting-filesystem-kqueue.cpp
  change-detecting-filesystem-win32.cpp
  char8.cpp
  cli-location.cpp
  configuration-loader.cpp
  configuration.cpp
  crash.cpp
  document.cpp
  emacs-lisp-error-reporter.cpp
  emacs-location.cpp
  error-collector.cpp
  error-formatter.cpp
  error-list.cpp
  file-canonical.cpp
  file-handle.cpp
  file-path.cpp
  file.cpp
  global-variables-browser.cpp
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
  lsp-error-reporter.cpp
  lsp-location.cpp
  lsp-message-parser.cpp
  lsp-pipe-writer.cpp
  lsp-server.cpp
  options.cpp
  padded-string.cpp
  parse.cpp
  pipe-writer.cpp
  quick-lint-js/assert.h
  quick-lint-js/basic-configuration-filesystem.h
  quick-lint-js/bit.h
  quick-lint-js/buffering-error-reporter.h
  quick-lint-js/buffering-visitor.h
  quick-lint-js/byte-buffer.h
  quick-lint-js/c-api-error-reporter.h
  quick-lint-js/c-api.h
  quick-lint-js/change-detecting-filesystem.h
  quick-lint-js/char8.h
  quick-lint-js/cli-location.h
  quick-lint-js/configuration-loader.h
  quick-lint-js/configuration.h
  quick-lint-js/constant-divider.h
  quick-lint-js/cpp.h
  quick-lint-js/crash.h
  quick-lint-js/document.h
  quick-lint-js/emacs-lisp-error-reporter.h
  quick-lint-js/emacs-location.h
  quick-lint-js/error-collector.h
  quick-lint-js/error-formatter.h
  quick-lint-js/error-list.h
  quick-lint-js/error-reporter.h
  quick-lint-js/error-tape.h
  quick-lint-js/error.h
  quick-lint-js/event-loop.h
  quick-lint-js/expression.h
  quick-lint-js/feature.h
  quick-lint-js/file-canonical.h
  quick-lint-js/file-handle.h
  quick-lint-js/file-path.h
  quick-lint-js/file.h
  quick-lint-js/force-inline.h
  quick-lint-js/global-variables.h
  quick-lint-js/gmo.h
  quick-lint-js/have.h
  quick-lint-js/integer.h
  quick-lint-js/json.h
  quick-lint-js/language.h
  quick-lint-js/lex.h
  quick-lint-js/lint.h
  quick-lint-js/locale.h
  quick-lint-js/location.h
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
  quick-lint-js/pipe-writer.h
  quick-lint-js/result.h
  quick-lint-js/simd.h
  quick-lint-js/temporary-directory.h
  quick-lint-js/text-error-reporter.h
  quick-lint-js/translation-data.h
  quick-lint-js/translation.h
  quick-lint-js/unreachable.h
  quick-lint-js/uri.h
  quick-lint-js/utf-16.h
  quick-lint-js/utf-8.h
  quick-lint-js/vector.h
  quick-lint-js/version.h
  quick-lint-js/vim-location.h
  quick-lint-js/vim-qflist-json-error-reporter.h
  quick-lint-js/warning.h
  quick-lint-js/web-demo-location.h
  temporary-directory.cpp
  text-error-reporter.cpp
  translation-data.cpp
  translation.cpp
  uri.cpp
  utf-16.cpp
  utf-8.cpp
  vector.cpp
  vim-location.cpp
  vim-qflist-json-error-reporter.cpp
  web-demo-location.cpp
)
target_include_directories(quick-lint-js-lib PUBLIC .)
target_link_libraries(quick-lint-js-lib PUBLIC boost simdjson)
if (WIN32)
  target_link_libraries(quick-lint-js-lib PUBLIC Pathcch)
endif ()
target_link_libraries(quick-lint-js-lib PUBLIC boost simdjson)
if (NEED_THREADS)
  target_link_libraries(quick-lint-js-lib PRIVATE Threads::Threads)
endif ()
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

quick_lint_js_have_charconv(QUICK_LINT_JS_HAVE_CHARCONV)
if (NOT QUICK_LINT_JS_HAVE_CHARCONV)
  target_compile_definitions(
    quick-lint-js-lib
    PUBLIC
    QLJS_HAVE_CHARCONV_HEADER=0
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
