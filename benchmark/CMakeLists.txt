# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

cmake_minimum_required(VERSION 3.10)
include(QuickLintJSTarget)

function (quick_lint_js_add_benchmark_executable TARGET_NAME)
  set(SOURCES "${ARGN}")
  quick_lint_js_add_executable("${TARGET_NAME}" ${SOURCES})
  target_link_libraries(
    "${TARGET_NAME}"
    PRIVATE
    benchmark::benchmark
    benchmark::benchmark_main
    quick-lint-js-benchmark-support
    quick-lint-js-lib
  )
endfunction ()

quick_lint_js_add_benchmark_executable(
  quick-lint-js-benchmark-cli-location
  benchmark-cli-location.cpp
)
quick_lint_js_add_benchmark_executable(
  quick-lint-js-benchmark-configuration
  benchmark-configuration.cpp
)
quick_lint_js_add_benchmark_executable(
  quick-lint-js-benchmark-configuration-loader
  benchmark-configuration-loader.cpp
)
quick_lint_js_add_benchmark_executable(
  quick-lint-js-benchmark-document
  benchmark-document.cpp
)
quick_lint_js_add_benchmark_executable(
  quick-lint-js-benchmark-lex
  benchmark-lex.cpp
)
quick_lint_js_add_benchmark_executable(
  quick-lint-js-benchmark-lint
  benchmark-lint.cpp
)
quick_lint_js_add_benchmark_executable(
  quick-lint-js-benchmark-lsp
  benchmark-lsp.cpp
)
quick_lint_js_add_benchmark_executable(
  quick-lint-js-benchmark-lsp-location
  benchmark-lsp-location.cpp
)
quick_lint_js_add_benchmark_executable(
  quick-lint-js-benchmark-parse
  benchmark-parse.cpp
)
quick_lint_js_add_benchmark_executable(
  quick-lint-js-benchmark-translation
  benchmark-translation.cpp
)
quick_lint_js_add_benchmark_executable(
  quick-lint-js-benchmark-utf-8
  benchmark-utf-8.cpp
)
quick_lint_js_add_benchmark_executable(
  quick-lint-js-benchmark-vim-location
  benchmark-vim-location.cpp
)

quick_lint_js_add_library(
  quick-lint-js-benchmark-support
  generate-code.cpp
  quick-lint-js/generate-code.h
)
target_include_directories(quick-lint-js-benchmark-support PUBLIC .)
target_link_libraries(quick-lint-js-benchmark-support PUBLIC quick-lint-js-lib)

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
