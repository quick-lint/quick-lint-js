# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

cmake_minimum_required(VERSION 3.10)
include(CheckCXXSourceCompiles)
include(CheckIncludeFileCXX)
include(QuickLintJSTarget)

check_include_file_cxx(
  experimental/coroutine
  QUICK_LINT_JS_HAVE_EXPERIMENTAL_COROUTINE
)
check_cxx_source_compiles(
  "#include <filesystem>
int main() {
  return std::filesystem::path(u8\"hello\").empty() ? 2 : 0;
}"
  QUICK_LINT_JS_HAVE_FILESYSTEM_CHAR8
)
if (QUICK_LINT_JS_HAVE_EXPERIMENTAL_COROUTINE AND QUICK_LINT_JS_HAVE_FILESYSTEM_CHAR8)
  quick_lint_js_add_executable(
    quick-lint-js-benchmark-lsp-servers
    benchmark-config.cpp
    lsp-benchmarks.cpp
    lsp-server-process.cpp
    main.cpp
    process.cpp
    quick-lint-js/benchmark-config.h
    quick-lint-js/lsp-benchmarks.h
    quick-lint-js/lsp-logging.h
    quick-lint-js/lsp-server-process.h
    quick-lint-js/process.h
  )
  target_link_libraries(
    quick-lint-js-benchmark-lsp-servers
    PRIVATE
    boost_json
    quick-lint-js-boost-json
    quick-lint-js-lib
    simdjson
  )
  target_include_directories(quick-lint-js-benchmark-lsp-servers PRIVATE .)
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
