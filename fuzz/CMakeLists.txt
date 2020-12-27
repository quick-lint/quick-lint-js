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
include(QuickLintJSTarget)

option(QUICK_LINT_JS_ENABLE_LLVM_LIBFUZZER_TESTS "" OFF)

if (QUICK_LINT_JS_ENABLE_LLVM_LIBFUZZER_TESTS)
  function (quick_lint_js_add_fuzz_executable TARGET)
    quick_lint_js_add_executable("${TARGET}" ${ARGN})
    target_compile_options("${TARGET}" PRIVATE -fsanitize=fuzzer)
    target_link_libraries("${TARGET}" PRIVATE -fsanitize=fuzzer)
  endfunction ()

  quick_lint_js_add_fuzz_executable(quick-lint-js-fuzz-lex fuzz-lex.cpp)
  target_link_libraries(quick-lint-js-fuzz-lex PRIVATE quick-lint-js-lib)

  quick_lint_js_add_fuzz_executable(quick-lint-js-fuzz-lsp-server fuzz-lsp-server.cpp)
  target_link_libraries(quick-lint-js-fuzz-lsp-server PRIVATE quick-lint-js-lib)

  quick_lint_js_add_fuzz_executable(quick-lint-js-fuzz-options fuzz-options.cpp)
  target_link_libraries(quick-lint-js-fuzz-options PRIVATE quick-lint-js-lib)

  quick_lint_js_add_fuzz_executable(quick-lint-js-fuzz-parse fuzz-parse.cpp)
  target_link_libraries(quick-lint-js-fuzz-parse PRIVATE quick-lint-js-lib)

  quick_lint_js_add_fuzz_executable(quick-lint-js-fuzz-utf-8-decode-encode fuzz-utf-8-decode-encode.cpp)
  target_link_libraries(quick-lint-js-fuzz-utf-8-decode-encode PRIVATE quick-lint-js-lib)

  quick_lint_js_add_fuzz_executable(quick-lint-js-fuzz-utf-8-lsp fuzz-utf-8-lsp.cpp)
  target_link_libraries(quick-lint-js-fuzz-utf-8-lsp PRIVATE quick-lint-js-lib)
endif ()
