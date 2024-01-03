# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

cmake_minimum_required(VERSION 3.13)
include(GNUInstallDirs)
include(QuickLintJSCompiler)
include(QuickLintJSCopyright)
include(QuickLintJSTarget)

find_package(Threads REQUIRED)

if (QUICK_LINT_JS_ENABLE_CLI)
  set(EXTRA_SOURCES)
  if (WIN32)
    list(APPEND EXTRA_SOURCES quick-lint-js/cli/quick-lint-js.rc)
    # TODO(strager): Add a dependency on dusty-icon.ico which is referenced by
    # quick-lint-js.rc.
  endif ()
  quick_lint_js_add_executable(
    quick-lint-js
    quick-lint-js/cli/main.cpp
    ${EXTRA_SOURCES}
  )
  set_target_properties(quick-lint-js PROPERTIES RUNTIME_OUTPUT_DIRECTORY ..)
  target_link_libraries(quick-lint-js PRIVATE quick-lint-js-lib)
  quick_lint_js_enable_windows_unicode(quick-lint-js)

  # TODO(strager): Use the default DESTINATION for CMake versions which have a
  # default. (3.18.0 has a default; 3.12.4 doesn't have a default.)
  install(TARGETS quick-lint-js RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
endif ()

if (TARGET quick-lint-js-generate-diagnostic-metadata)
  add_custom_command(
    OUTPUT
    "${CMAKE_CURRENT_SOURCE_DIR}/quick-lint-js/diag/diagnostic-metadata-generated.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/quick-lint-js/diag/diagnostic-metadata-generated.h"
    COMMAND
      $<TARGET_FILE:quick-lint-js-generate-diagnostic-metadata>
      --output-info-cpp "${CMAKE_CURRENT_SOURCE_DIR}/quick-lint-js/diag/diagnostic-metadata-generated.cpp"
      --output-type-list-h "${CMAKE_CURRENT_SOURCE_DIR}/quick-lint-js/diag/diagnostic-metadata-generated.h"
      "${CMAKE_CURRENT_SOURCE_DIR}/quick-lint-js/diag/diagnostic-types-2.h"
    DEPENDS
      quick-lint-js-generate-diagnostic-metadata
      "${CMAKE_CURRENT_SOURCE_DIR}/quick-lint-js/diag/diagnostic-types-2.h"
    COMMENT "Generating metadata of diagnostics for diagnostic-metadata-generated.h"
  )
endif ()
# NOTE[generated-target]: We must create a target just for these generated
# files. Otherwise, Ninja's build gets order-only dependencies from
# diagnostic-metadata-generated.h to libquick-lint-js-debug.a, forming a
# dependency cycle (because libquick-lint-js-debug.a depends on
# diagnostic-types.h which depends on diagnostic-metadata-generated.h).
#
# DEPENDS_EXPLICIT_ONLY (CMake 3.27) might fix this, removing the need for an
# intermediate target.
quick_lint_js_add_library(
  quick-lint-js-diagnostic-metadata
  STATIC
  quick-lint-js/diag/diagnostic-metadata-generated.cpp
  quick-lint-js/diag/diagnostic-metadata-generated.h
)
target_include_directories(quick-lint-js-diagnostic-metadata PUBLIC .)
target_link_libraries(
  quick-lint-js-diagnostic-metadata
  PUBLIC
  quick-lint-js-i18n
)
quick_lint_js_target_add_cxx_flag_if_supported(
  quick-lint-js-diagnostic-metadata
  /constexpr:steps9999999
  QUICK_LINT_JS_HAVE_CONSTEXPR_STEPS
)
quick_lint_js_target_add_cxx_flag_if_supported(
  quick-lint-js-diagnostic-metadata
  -fconstexpr-steps=9999999
  QUICK_LINT_JS_HAVE_FCONSTEXPR_STEPS
)

if (TARGET quick-lint-js-generate-lex-keyword)
  add_custom_command(
    OUTPUT "${CMAKE_CURRENT_SOURCE_DIR}/quick-lint-js/fe/lex-keyword-generated.cpp"
    COMMAND
      $<TARGET_FILE:quick-lint-js-generate-lex-keyword>
      --output
      "${CMAKE_CURRENT_SOURCE_DIR}/quick-lint-js/fe/lex-keyword-generated.cpp"
    DEPENDS quick-lint-js-generate-lex-keyword
    COMMENT "Generating keyword hash table for lex-keyword-generated.cpp"
  )
endif ()

if (TARGET quick-lint-js-generate-trace-sources)
  add_custom_command(
    OUTPUT
    "${CMAKE_CURRENT_SOURCE_DIR}/quick-lint-js/debug/public/trace-generated.mjs"
    "${CMAKE_CURRENT_SOURCE_DIR}/quick-lint-js/logging/trace-metadata-generated.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/quick-lint-js/logging/trace-reader-generated.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/quick-lint-js/logging/trace-reader-generated.h"
    "${CMAKE_CURRENT_SOURCE_DIR}/quick-lint-js/logging/trace-writer-generated.h"
    "${CMAKE_CURRENT_SOURCE_DIR}/../tools/trace.bt"
    COMMAND
      $<TARGET_FILE:quick-lint-js-generate-trace-sources>
      --output-010-editor-template "${CMAKE_CURRENT_SOURCE_DIR}/../tools/trace.bt"
      --output-metadata-cpp "${CMAKE_CURRENT_SOURCE_DIR}/quick-lint-js/logging/trace-metadata-generated.cpp"
      --output-parser-cpp "${CMAKE_CURRENT_SOURCE_DIR}/quick-lint-js/logging/trace-reader-generated.cpp"
      --output-parser-h "${CMAKE_CURRENT_SOURCE_DIR}/quick-lint-js/logging/trace-reader-generated.h"
      --output-parser-js "${CMAKE_CURRENT_SOURCE_DIR}/quick-lint-js/debug/public/trace-generated.mjs"
      --output-writer-h "${CMAKE_CURRENT_SOURCE_DIR}/quick-lint-js/logging/trace-writer-generated.h"
      --trace-types-h "${CMAKE_CURRENT_SOURCE_DIR}/quick-lint-js/logging/trace-types.h"
    DEPENDS
      quick-lint-js-generate-trace-sources
      "${CMAKE_CURRENT_SOURCE_DIR}/quick-lint-js/logging/trace-types.h"
    COMMENT "Generating metadata files, parsers, and writers for tracing"
  )
endif ()
# See NOTE[generated-target].
quick_lint_js_add_library(
  quick-lint-js-trace
  STATIC
  quick-lint-js/logging/trace-metadata-generated.cpp
  quick-lint-js/logging/trace-reader-generated.cpp
  quick-lint-js/logging/trace-reader-generated.h
  quick-lint-js/logging/trace-writer-generated.h
)
target_include_directories(quick-lint-js-trace PUBLIC .)
target_link_libraries(quick-lint-js-trace PUBLIC quick-lint-js-tool-lib)

# C++ files which contain QLJS_TRANSLATABLE directives.
set(
  QLJS_TRANSLATION_SOURCES
  ../benchmark/benchmark-translation.cpp
  quick-lint-js/diag/diagnostic-formatter.cpp
  quick-lint-js/diag/diagnostic-metadata-generated.cpp
  quick-lint-js/diag/diagnostic-metadata-generated.h
  ../test/test-diagnostic-formatter.cpp
  ../test/test-vim-qflist-json-diag-reporter.cpp
)

# GNU gettext .po files which contain translation information.
set(
  QLJS_TRANSLATION_FILES
  ../po/de.po
  ../po/en_US@snarky.po
  ../po/fr_FR.po
  ../po/pt_BR.po
  ../po/sv_SE.po
)

if (TARGET quick-lint-js-compile-translations)
  set(COMMAND $<TARGET_FILE:quick-lint-js-compile-translations>)
  set(DEPENDS quick-lint-js-compile-translations)
  set(OUTPUTS)
  set(WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/..")

  macro (add_flag LIST FLAG PATH)
    # Make the output of compile-translations deterministic by using relative
    # paths.
    file(RELATIVE_PATH _PATH "${WORKING_DIRECTORY}" "${CMAKE_CURRENT_SOURCE_DIR}/${PATH}")
    list(APPEND COMMAND "${FLAG}" "${_PATH}")
    list(APPEND "${LIST}" "${CMAKE_CURRENT_SOURCE_DIR}/${PATH}")
  endmacro ()

  foreach (SOURCE IN LISTS QLJS_TRANSLATION_SOURCES)
    add_flag(DEPENDS --source "${SOURCE}")
  endforeach ()
  foreach (TRANSLATION_FILE IN LISTS QLJS_TRANSLATION_FILES)
    add_flag(DEPENDS --po "${TRANSLATION_FILE}")
  endforeach ()
  add_flag(OUTPUTS --output-messages-pot ../po/messages.pot)
  add_flag(OUTPUTS --output-translation-table-cpp quick-lint-js/i18n/translation-table-generated.cpp)
  add_flag(OUTPUTS --output-translation-table-h quick-lint-js/i18n/translation-table-generated.h)
  add_flag(OUTPUTS --output-translation-table-test quick-lint-js/i18n/translation-table-test-generated.h)

  add_custom_command(
    OUTPUT ${OUTPUTS}
    COMMAND ${COMMAND}
    DEPENDS ${DEPENDS}
    COMMENT "Scanning sources for translatable strings and generating translation tables"
    WORKING_DIRECTORY "${WORKING_DIRECTORY}"
  )
endif ()
# See NOTE[generated-target].
quick_lint_js_add_library(
  quick-lint-js-i18n
  STATIC
  quick-lint-js/i18n/translation-table-generated.cpp
  quick-lint-js/i18n/translation-table-generated.h
  quick-lint-js/i18n/translation-table-test-generated.h
)
target_include_directories(quick-lint-js-i18n PUBLIC .)

# These files are needed by some helper tools in addition to the core.
quick_lint_js_add_library(
  quick-lint-js-tool-lib
  quick-lint-js/assert.cpp
  quick-lint-js/assert.h
  quick-lint-js/cli/arg-parser.cpp
  quick-lint-js/cli/arg-parser.h
  quick-lint-js/cli/cli-location.cpp
  quick-lint-js/cli/cli-location.h
  quick-lint-js/container/flexible-array.h
  quick-lint-js/container/linked-bump-allocator.cpp
  quick-lint-js/container/linked-bump-allocator.h
  quick-lint-js/container/padded-string.cpp
  quick-lint-js/container/padded-string.h
  quick-lint-js/container/string-view.h
  quick-lint-js/container/vector-profiler.cpp
  quick-lint-js/container/vector-profiler.h
  quick-lint-js/container/vector.h
  quick-lint-js/container/winkable.h
  quick-lint-js/fe/keyword-lexer.h
  quick-lint-js/fe/keyword-list.h
  quick-lint-js/i18n/po-parser.cpp
  quick-lint-js/i18n/po-parser.h
  quick-lint-js/i18n/translation-table-compiler.cpp
  quick-lint-js/i18n/translation-table-compiler.h
  quick-lint-js/io/file-handle-posix.cpp
  quick-lint-js/io/file-handle-win32.cpp
  quick-lint-js/io/file-handle.h
  quick-lint-js/io/file-path.cpp
  quick-lint-js/io/file-path.h
  quick-lint-js/io/file.cpp
  quick-lint-js/io/file.h
  quick-lint-js/io/io-error.h
  quick-lint-js/io/output-stream.cpp
  quick-lint-js/io/output-stream.h
  quick-lint-js/port/char8.cpp
  quick-lint-js/port/char8.h
  quick-lint-js/port/have.h
  quick-lint-js/port/in-range.h
  quick-lint-js/port/limits.h
  quick-lint-js/port/math.h
  quick-lint-js/port/memory-resource.cpp
  quick-lint-js/port/memory-resource.h
  quick-lint-js/port/source-location.h
  quick-lint-js/port/thread.cpp
  quick-lint-js/port/thread.h
  quick-lint-js/port/type-traits.h
  quick-lint-js/port/warning.h
  quick-lint-js/port/windows-error.cpp
  quick-lint-js/port/windows-error.h
  quick-lint-js/port/windows.h
  quick-lint-js/reflection/cxx-parser.cpp
  quick-lint-js/reflection/cxx-parser.h
  quick-lint-js/util/cast.h
  quick-lint-js/util/enum.h
  quick-lint-js/util/float.cpp
  quick-lint-js/util/float.h
  quick-lint-js/util/integer.cpp
  quick-lint-js/util/integer.h
  quick-lint-js/util/utf-16.cpp
  quick-lint-js/util/utf-16.h
  quick-lint-js/util/utf-8.cpp
  quick-lint-js/util/utf-8.h
)
target_include_directories(quick-lint-js-tool-lib PUBLIC .)
if (WIN32)
  target_link_libraries(quick-lint-js-tool-lib PUBLIC pathcch)
endif ()

quick_lint_js_add_library(
  quick-lint-js-lib
  quick-lint-js/c-api-diag-reporter.cpp
  quick-lint-js/c-api-diag-reporter.h
  quick-lint-js/c-api.cpp
  quick-lint-js/c-api.h
  quick-lint-js/cli/emacs-lisp-diag-reporter.cpp
  quick-lint-js/cli/emacs-lisp-diag-reporter.h
  quick-lint-js/cli/emacs-location.cpp
  quick-lint-js/cli/emacs-location.h
  quick-lint-js/cli/options.cpp
  quick-lint-js/cli/options.h
  quick-lint-js/cli/text-diag-reporter.cpp
  quick-lint-js/cli/text-diag-reporter.h
  quick-lint-js/cli/vim-location.cpp
  quick-lint-js/cli/vim-location.h
  quick-lint-js/cli/vim-qflist-json-diag-reporter.cpp
  quick-lint-js/cli/vim-qflist-json-diag-reporter.h
  quick-lint-js/configuration/basic-configuration-filesystem.cpp
  quick-lint-js/configuration/basic-configuration-filesystem.h
  quick-lint-js/configuration/change-detecting-filesystem-inotify.cpp
  quick-lint-js/configuration/change-detecting-filesystem-kqueue.cpp
  quick-lint-js/configuration/change-detecting-filesystem-win32.cpp
  quick-lint-js/configuration/change-detecting-filesystem.cpp
  quick-lint-js/configuration/change-detecting-filesystem.h
  quick-lint-js/configuration/configuration-loader.cpp
  quick-lint-js/configuration/configuration-loader.h
  quick-lint-js/configuration/configuration.cpp
  quick-lint-js/configuration/configuration.h
  quick-lint-js/container/allocator.h
  quick-lint-js/container/async-byte-queue.cpp
  quick-lint-js/container/async-byte-queue.h
  quick-lint-js/container/byte-buffer.cpp
  quick-lint-js/container/byte-buffer.h
  quick-lint-js/container/c-string-list.h
  quick-lint-js/container/concat.h
  quick-lint-js/container/fixed-vector.h
  quick-lint-js/container/hash-map.h
  quick-lint-js/container/hash-set.h
  quick-lint-js/container/hash.h
  quick-lint-js/container/linked-vector.h
  quick-lint-js/container/optional.h
  quick-lint-js/container/result.h
  quick-lint-js/debug/debug-probe.cpp
  quick-lint-js/debug/debug-probe.h
  quick-lint-js/debug/debug-server-fs.cpp
  quick-lint-js/debug/debug-server-fs.h
  quick-lint-js/debug/debug-server.cpp
  quick-lint-js/debug/debug-server.h
  quick-lint-js/debug/find-debug-server.cpp
  quick-lint-js/debug/find-debug-server.h
  quick-lint-js/debug/mongoose.cpp
  quick-lint-js/debug/mongoose.h
  quick-lint-js/diag/buffering-diag-reporter.cpp
  quick-lint-js/diag/buffering-diag-reporter.h
  quick-lint-js/diag/diag-code-list.cpp
  quick-lint-js/diag/diag-code-list.h
  quick-lint-js/diag/diag-list.cpp
  quick-lint-js/diag/diag-list.h
  quick-lint-js/diag/diag-reporter.cpp
  quick-lint-js/diag/diag-reporter.h
  quick-lint-js/diag/diagnostic-formatter.cpp
  quick-lint-js/diag/diagnostic-formatter.h
  quick-lint-js/diag/diagnostic-types.h
  quick-lint-js/diag/diagnostic.cpp
  quick-lint-js/diag/diagnostic.h
  quick-lint-js/diag/reported-diag-statistics.h
  quick-lint-js/fe/buffering-visitor.cpp
  quick-lint-js/fe/buffering-visitor.h
  quick-lint-js/fe/expression.h
  quick-lint-js/fe/global-declared-variable-set.cpp
  quick-lint-js/fe/global-declared-variable-set.h
  quick-lint-js/fe/global-variables-browser-generated.cpp
  quick-lint-js/fe/global-variables.cpp
  quick-lint-js/fe/global-variables.h
  quick-lint-js/fe/identifier.h
  quick-lint-js/fe/jsx.cpp
  quick-lint-js/fe/jsx.h
  quick-lint-js/fe/keyword-lexer.cpp
  quick-lint-js/fe/language.cpp
  quick-lint-js/fe/language.h
  quick-lint-js/fe/lex-keyword-generated.cpp
  quick-lint-js/fe/lex-unicode-generated.cpp
  quick-lint-js/fe/lex.cpp
  quick-lint-js/fe/lex.h
  quick-lint-js/fe/linter.cpp
  quick-lint-js/fe/linter.h
  quick-lint-js/fe/multi-parse-visitor.h
  quick-lint-js/fe/null-visitor.cpp
  quick-lint-js/fe/null-visitor.h
  quick-lint-js/fe/parse-class.cpp
  quick-lint-js/fe/parse-expression.cpp
  quick-lint-js/fe/parse-statement.cpp
  quick-lint-js/fe/parse-type.cpp
  quick-lint-js/fe/parse-visitor.h
  quick-lint-js/fe/parse.cpp
  quick-lint-js/fe/parse.h
  quick-lint-js/fe/source-code-span-debug.cpp
  quick-lint-js/fe/source-code-span.cpp
  quick-lint-js/fe/source-code-span.h
  quick-lint-js/fe/token.cpp
  quick-lint-js/fe/token.h
  quick-lint-js/fe/variable-analyzer.cpp
  quick-lint-js/fe/variable-analyzer.h
  quick-lint-js/feature.h
  quick-lint-js/i18n/locale.cpp
  quick-lint-js/i18n/locale.h
  quick-lint-js/i18n/translation-table.h
  quick-lint-js/i18n/translation.cpp
  quick-lint-js/i18n/translation.h
  quick-lint-js/io/event-loop-kqueue.cpp
  quick-lint-js/io/event-loop-poll.cpp
  quick-lint-js/io/event-loop-windows.cpp
  quick-lint-js/io/event-loop.cpp
  quick-lint-js/io/event-loop.h
  quick-lint-js/io/file-canonical.cpp
  quick-lint-js/io/file-canonical.h
  quick-lint-js/io/pipe-writer.cpp
  quick-lint-js/io/pipe-writer.h
  quick-lint-js/io/pipe.cpp
  quick-lint-js/io/pipe.h
  quick-lint-js/io/temporary-directory.cpp
  quick-lint-js/io/temporary-directory.h
  quick-lint-js/json.cpp
  quick-lint-js/json.h
  quick-lint-js/logging/log.h
  quick-lint-js/logging/logger.cpp
  quick-lint-js/logging/logger.h
  quick-lint-js/logging/trace-flusher-directory-backend.cpp
  quick-lint-js/logging/trace-flusher.cpp
  quick-lint-js/logging/trace-flusher.h
  quick-lint-js/logging/trace-metadata.h
  quick-lint-js/logging/trace-reader.cpp
  quick-lint-js/logging/trace-reader.h
  quick-lint-js/logging/trace-types.cpp
  quick-lint-js/logging/trace-types.h
  quick-lint-js/logging/trace-writer.cpp
  quick-lint-js/logging/trace-writer.h
  quick-lint-js/lsp/lsp-diag-reporter.cpp
  quick-lint-js/lsp/lsp-diag-reporter.h
  quick-lint-js/lsp/lsp-document-text.cpp
  quick-lint-js/lsp/lsp-document-text.h
  quick-lint-js/lsp/lsp-json-rpc-message-parser.cpp
  quick-lint-js/lsp/lsp-json-rpc-message-parser.h
  quick-lint-js/lsp/lsp-language.h
  quick-lint-js/lsp/lsp-location.cpp
  quick-lint-js/lsp/lsp-location.h
  quick-lint-js/lsp/lsp-message-parser.cpp
  quick-lint-js/lsp/lsp-message-parser.h
  quick-lint-js/lsp/lsp-pipe-writer.cpp
  quick-lint-js/lsp/lsp-pipe-writer.h
  quick-lint-js/lsp/lsp-server.cpp
  quick-lint-js/lsp/lsp-server.h
  quick-lint-js/lsp/lsp-uri.cpp
  quick-lint-js/lsp/lsp-uri.h
  quick-lint-js/lsp/lsp-workspace-configuration.cpp
  quick-lint-js/lsp/lsp-workspace-configuration.h
  quick-lint-js/lsp/outgoing-json-rpc-message-queue.cpp
  quick-lint-js/lsp/outgoing-json-rpc-message-queue.h
  quick-lint-js/port/attribute.h
  quick-lint-js/port/bit.h
  quick-lint-js/port/child-process.cpp
  quick-lint-js/port/child-process.h
  quick-lint-js/port/consteval.h
  quick-lint-js/port/constinit.h
  quick-lint-js/port/crash.h
  quick-lint-js/port/endian.cpp
  quick-lint-js/port/endian.h
  quick-lint-js/port/function-ref.h
  quick-lint-js/port/kqueue.h
  quick-lint-js/port/max-align.h
  quick-lint-js/port/process.cpp
  quick-lint-js/port/process.h
  quick-lint-js/port/pty.cpp
  quick-lint-js/port/pty.h
  quick-lint-js/port/simd.h
  quick-lint-js/port/span.h
  quick-lint-js/port/thread-name.cpp
  quick-lint-js/port/thread-name.h
  quick-lint-js/port/unreachable.h
  quick-lint-js/simdjson.cpp
  quick-lint-js/simdjson.h
  quick-lint-js/util/ascii.h
  quick-lint-js/util/binary-reader.h
  quick-lint-js/util/binary-writer.h
  quick-lint-js/util/classify-path.cpp
  quick-lint-js/util/classify-path.h
  quick-lint-js/util/cpp.h
  quick-lint-js/util/instance-tracker.h
  quick-lint-js/util/math-overflow.h
  quick-lint-js/util/pointer.h
  quick-lint-js/util/synchronized.h
  quick-lint-js/util/try-catch-stack.h
  quick-lint-js/util/type-traits.h
  quick-lint-js/util/uri.cpp
  quick-lint-js/util/uri.h
  quick-lint-js/version.h
  quick-lint-js/vscode/vscode-language.h
  quick-lint-js/web-demo-location.cpp
  quick-lint-js/web-demo-location.h
)
target_compile_definitions(
  quick-lint-js-lib
  PUBLIC
  "QUICK_LINT_JS_VERSION=${QUICK_LINT_JS_VERSION}"
)
target_include_directories(quick-lint-js-lib PUBLIC .)
target_link_libraries(
  quick-lint-js-lib
  PUBLIC
  quick-lint-js-diagnostic-metadata
  quick-lint-js-i18n
  quick-lint-js-tool-lib
  quick-lint-js-trace
  simdjson
  PRIVATE
  Threads::Threads
)
if (WIN32)
  target_link_libraries(quick-lint-js-lib PUBLIC pathcch)
endif ()
if (QUICK_LINT_JS_FEATURE_DEBUG_SERVER)
  target_link_libraries(quick-lint-js-lib PUBLIC mongoose)
endif ()
if (CMAKE_SYSTEM_NAME STREQUAL FreeBSD)
  target_link_libraries(quick-lint-js-lib PUBLIC kvm)
endif ()

if (QUICK_LINT_JS_FEATURE_DEBUG_SERVER)
  target_compile_definitions(
    quick-lint-js-lib
    PUBLIC
    QLJS_FEATURE_DEBUG_SERVER=1
  )
else ()
  target_compile_definitions(
    quick-lint-js-lib
    PUBLIC
    QLJS_FEATURE_DEBUG_SERVER=0
  )
endif ()

if (QUICK_LINT_JS_FEATURE_VECTOR_PROFILING)
  target_compile_definitions(
    quick-lint-js-tool-lib
    PUBLIC
    QLJS_FEATURE_VECTOR_PROFILING=1
  )
else ()
  target_compile_definitions(
    quick-lint-js-tool-lib
    PUBLIC
    QLJS_FEATURE_VECTOR_PROFILING=0
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
  quick-lint-js/cli/cli-location-debug.cpp
  quick-lint-js/cli/emacs-location-debug.cpp
  quick-lint-js/cli/vim-location-debug.cpp
  quick-lint-js/container/padded-string-debug.cpp
  quick-lint-js/container/vector-profiler-debug.cpp
  quick-lint-js/diag/diag-debug.cpp
  quick-lint-js/fe/language-debug.cpp
  quick-lint-js/fe/lex-debug.cpp
  quick-lint-js/i18n/po-parser-debug.cpp
  quick-lint-js/lsp/lsp-location-debug.cpp
  quick-lint-js/port/char8-debug.cpp
)
target_link_libraries(quick-lint-js-lib-debug PUBLIC quick-lint-js-lib)

if (QUICK_LINT_JS_ENABLE_CLI)
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
