# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

cmake_minimum_required(VERSION 3.13)
if (POLICY CMP0091)
  cmake_policy(SET CMP0091 NEW)
endif ()

file(READ version QUICK_LINT_JS_VERSION)
string(REGEX MATCH "^[^\n]+" QUICK_LINT_JS_VERSION "${QUICK_LINT_JS_VERSION}")

project(quick-lint-js)

include(GNUInstallDirs)

option(QUICK_LINT_JS_ENABLE_BENCHMARKS "Build microbenchmark programs" OFF)
option(QUICK_LINT_JS_ENABLE_VSCODE "Build VS Code extension bits" OFF)

set(QUICK_LINT_JS_ENABLE_CLI_DEFAULT ON)
if (EMSCRIPTEN)
  set(QUICK_LINT_JS_ENABLE_CLI_DEFAULT OFF)
endif ()
option(
  QUICK_LINT_JS_ENABLE_CLI
  "Build the command-line program and LSP server"
  "${QUICK_LINT_JS_ENABLE_CLI_DEFAULT}"
)

set(
  QUICK_LINT_JS_USE_BUILD_TOOLS
  ""
  CACHE
  PATH
  "Directory containing tools built by the quick-lint-js-code-gen-tools target. See http://quick-lint-js.com/contribute/build-from-source/cross-compiling/ for details."
)

set(QUICK_LINT_JS_ENABLE_BUILD_TOOLS_DEFAULT ON)
if (QUICK_LINT_JS_USE_BUILD_TOOLS OR EMSCRIPTEN OR CMAKE_CROSSCOMPILING)
  set(QUICK_LINT_JS_ENABLE_BUILD_TOOLS_DEFAULT OFF)
endif ()
option(
  QUICK_LINT_JS_ENABLE_BUILD_TOOLS
  "Compile tools which generate quick-lint-js source code and use them during the build"
  "${QUICK_LINT_JS_ENABLE_BUILD_TOOLS_DEFAULT}"
)

if (EMSCRIPTEN)
  # TODO(strager): Require QUICK_LINT_JS_USE_BUILD_TOOLS or
  # QUICK_LINT_JS_ENABLE_BUILD_TOOLS for Emscripten builds.
else ()
  if (NOT (QUICK_LINT_JS_USE_BUILD_TOOLS OR QUICK_LINT_JS_ENABLE_BUILD_TOOLS))
    set(MESSAGE "Neither QUICK_LINT_JS_USE_BUILD_TOOLS nor QUICK_LINT_JS_ENABLE_BUILD_TOOLS was specified.")
    if (CMAKE_CROSSCOMPILING OR EMSCRIPTEN)
      set(MESSAGE "${MESSAGE} QUICK_LINT_JS_ENABLE_BUILD_TOOLS was automatically disabled because you are cross-compiling.")
    endif ()
    set(MESSAGE "${MESSAGE} You should configure with QUICK_LINT_JS_ENABLE_BUILD_TOOLS=YES for your build machine and build quick-lint-js-build-tools,")
    set(MESSAGE "${MESSAGE} then you should separately configure with QUICK_LINT_JS_USE_BUILD_TOOLS=(path to your first build) for your target machine.")
    set(MESSAGE "${MESSAGE} See this cross-compiling documentation for details: https://quick-lint-js.com/contribute/build-from-source/cross-compiling/")
    message(FATAL_ERROR "${MESSAGE}")
  endif ()
endif ()

if (QUICK_LINT_JS_ENABLE_BUILD_TOOLS AND QUICK_LINT_JS_USE_BUILD_TOOLS)
  message(FATAL_ERROR "QUICK_LINT_JS_ENABLE_BUILD_TOOLS and QUICK_LINT_JS_USE_BUILD_TOOLS were both specified, but are mutually exclusive. If QUICK_LINT_JS_USE_BUILD_TOOLS (${QUICK_LINT_JS_USE_BUILD_TOOLS}) is intended, set QUICK_LINT_JS_ENABLE_BUILD_TOOLS=NO.")
endif ()

if (${CMAKE_VERSION} VERSION_GREATER_EQUAL 3.17.3)
  option(QUICK_LINT_JS_PRECOMPILE_HEADERS "Use CMake's target_precompile_headers" ON)
else ()
  set(QUICK_LINT_JS_PRECOMPILE_HEADERS FALSE)
endif ()

set(
  QUICK_LINT_JS_INSTALL_EMACS_DIR
  "${CMAKE_INSTALL_DATAROOTDIR}/emacs/site-lisp"
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

option(
  QUICK_LINT_JS_USE_BUNDLED_GOOGLE_BENCHMARK
  "Use the third-party Google Benchmark library bundled with quick-lint-js instead of using the host-installed Google Benchmark"
  ON
)
option(
  QUICK_LINT_JS_USE_BUNDLED_GOOGLE_TEST
  "Use the third-party Google Test library bundled with quick-lint-js instead of using the host-installed Google Test"
  ON
)
option(
  QUICK_LINT_JS_USE_BUNDLED_SIMDJSON
  "Use the third-party simdjson library bundled with quick-lint-js instead of using the host-installed simdjson"
  ON
)

option(
  QUICK_LINT_JS_FEATURE_DEBUG_SERVER
  "Enable the HTTP monitoring server at run-time"
  FALSE
)
option(
  QUICK_LINT_JS_FEATURE_EXTRA_DEBUG
  "Enable extra slow debug checking at run-time"
  FALSE
)
option(
  QUICK_LINT_JS_FEATURE_VECTOR_PROFILING
  "Enable the QLJS_DUMP_VECTORS option at run-time"
  FALSE
)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

include(QuickLintJSCompiler)

quick_lint_js_set_cxx_standard()
quick_lint_js_enable_char8_t_if_supported()
quick_lint_js_configure_exception_handling()
quick_lint_js_configure_rtti()
quick_lint_js_work_around_implicit_link_directories()
quick_lint_js_work_around_precompiled_headers_pic_pie_conflict()
quick_lint_js_enable_dead_code_stripping()
quick_lint_js_disable_unwanted_warnings()

if (WIN32)
  add_definitions(-D_WIN32_WINNT=0x0602)
endif ()
if (QUICK_LINT_JS_FEATURE_EXTRA_DEBUG)
  add_compile_definitions(QLJS_DEBUG)
endif ()

# HACK(strager): Work around issues with CI. We should consider using
# find_package(Python3) instead.
find_package(PythonInterp 3.7)

find_package(PythonInterp 3)  # Force Python 3 (instead of Python 2).

# quick-lint-js-precompiled-headers is used by quick_lint_js_add_executable and
# quick_lint_js_add_library.
include(QuickLintJSTarget)
find_package(Threads REQUIRED)
quick_lint_js_add_library(
  quick-lint-js-precompiled-headers
  STATIC
  website/wasm/empty.cpp
)
target_link_libraries(
  quick-lint-js-precompiled-headers
  PUBLIC
  Threads::Threads
  simdjson
)
if (QUICK_LINT_JS_PRECOMPILE_HEADERS)
  target_precompile_headers(
    quick-lint-js-precompiled-headers
    PUBLIC
    <cmath>
    <cstring>
    <simdjson.h>
    <string>
  )
endif ()

# 'tools' must come first because it optionally defines targets. Other
# CMakeLists.txt files detect the presence of these targets.
add_subdirectory(tools)

if (QUICK_LINT_JS_ENABLE_BENCHMARKS)
  include(vendor/benchmark.cmake)
endif ()
include(vendor/googletest.cmake)
if (QUICK_LINT_JS_FEATURE_DEBUG_SERVER)
  include(vendor/mongoose.cmake)
endif ()
include(vendor/node-addon-api.cmake)
include(vendor/node.cmake)
include(vendor/simdjson.cmake)

if (QUICK_LINT_JS_ENABLE_BENCHMARKS)
  add_subdirectory(benchmark)
endif ()
add_subdirectory(dist)
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
