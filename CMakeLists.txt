# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

cmake_minimum_required(VERSION 3.10)
if (POLICY CMP0091)
  cmake_policy(SET CMP0091 NEW)
endif ()

file(READ version QUICK_LINT_JS_VERSION)
string(REGEX MATCH "^[^\n]+" QUICK_LINT_JS_VERSION "${QUICK_LINT_JS_VERSION}")

project(quick-lint-js)

include(GNUInstallDirs)

option(QUICK_LINT_JS_ENABLE_BENCHMARKS "Build microbenchmark programs" OFF)
option(QUICK_LINT_JS_ENABLE_VSCODE "Build VS Code extension bits" OFF)

if (${CMAKE_VERSION} VERSION_GREATER_EQUAL 3.17.3)
  option(QUICK_LINT_JS_PRECOMPILE_HEADERS "Use CMake's target_precompile_headers" ON)
else ()
  set(QUICK_LINT_JS_PRECOMPILE_HEADERS FALSE)
endif ()

set(
  QUICK_LINT_JS_INSTALL_EMACS_DIR
  "${CMAKE_INSTALL_DATAROOTDIR}/emacs/site-elisp"
  CACHE
  STRING
  "Install Emacs Lisp files into this directory relative to CMAKE_INSTALL_PREFIX"
)
set(
  QUICK_LINT_JS_INSTALL_LICENSES_DIR
  "${CMAKE_INSTALL_DOCDIR}"
  CACHE
  STRING
  "Install licenses into this directory relative to CMAKE_INSTALL_PREFIX"
)
set(
  QUICK_LINT_JS_INSTALL_NEOVIM_DIR
  "${CMAKE_INSTALL_DATAROOTDIR}/nvim/site"
  CACHE
  STRING
  "Install Neovim files into this directory relative to CMAKE_INSTALL_PREFIX"
)
set(
  QUICK_LINT_JS_INSTALL_VIM_DIR
  "${CMAKE_INSTALL_DATAROOTDIR}/vim/vimfiles"
  CACHE
  STRING
  "Install Vim files into this directory relative to CMAKE_INSTALL_PREFIX"
)
option(
  QUICK_LINT_JS_INSTALL_VIM_NEOVIM_TAGS
  "Install a 'tags' file with Vim and Neovim documentation"
  OFF
)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

include(QuickLintJSCompiler)

quick_lint_js_set_cxx_standard()
quick_lint_js_enable_char8_t_if_supported()
quick_lint_js_configure_exception_handling()
quick_lint_js_configure_rtti()
quick_lint_js_work_around_implicit_link_directories()
quick_lint_js_enable_dead_code_stripping()

# HACK(strager): Work around issues with CI. We should consider using
# find_package(Python3) instead.
find_package(PythonInterp 3.7)

find_package(PythonInterp 3)  # Force Python 3 (instead of Python 2).

if (QUICK_LINT_JS_ENABLE_BENCHMARKS)
  include(vendor/benchmark.cmake)
endif ()
include(vendor/boost.cmake)
include(vendor/googletest.cmake)
include(vendor/node-addon-api.cmake)
include(vendor/node.cmake)
include(vendor/simdjson.cmake)

if (QUICK_LINT_JS_ENABLE_BENCHMARKS)
  add_subdirectory(benchmark)
endif ()
add_subdirectory(docs)
add_subdirectory(fuzz)
add_subdirectory(plugin/vim)
add_subdirectory(plugin/emacs)
add_subdirectory(plugin/vscode)
add_subdirectory(src)
add_subdirectory(website/wasm)
add_subdirectory(completions)
include(CTest)
if (BUILD_TESTING)
  add_subdirectory(test)
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
