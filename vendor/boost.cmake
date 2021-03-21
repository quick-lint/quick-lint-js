# Copyright (C) 2020  Matthew Glazar
# See end of file for extended copyright information.

include(CheckCXXCompilerFlag)
include(QuickLintJSCompiler)

set(BUILD_TESTING FALSE)

set(
  BOOST_PROJECTS
  assert
  config
  container
  container_hash
  core
  detail
  integer
  intrusive
  move
  preprocessor
  static_assert
  throw_exception
  type_traits
)
foreach (BOOST_PROJECT IN LISTS BOOST_PROJECTS)
  add_subdirectory("boost/libs/${BOOST_PROJECT}")
endforeach ()
# NOTE(strager): SYSTEM disable undesirable warnings in Boost header files.
target_include_directories(boost_config SYSTEM INTERFACE boost)

# Disable undesirable warnings in headers and source files.
quick_lint_js_add_warning_options_if_supported(
  boost_container
  PRIVATE
  -Wno-null-pointer-arithmetic
)
foreach (BOOST_PROJECT IN LISTS BOOST_PROJECTS)
  quick_lint_js_add_warning_options_if_supported(
    "boost_${BOOST_PROJECT}"
    INTERFACE
    -Wno-missing-include-dirs
  )
endforeach ()

# Allow the entire project to be compiled with -fno-rtti. Boost uses
# dynamic_cast which requires RTTI, so forcefully enable RTTI for Boost.
check_cxx_compiler_flag(-frtti QUICK_LINT_JS_HAVE_FRTTI)
if (QUICK_LINT_JS_HAVE_FRTTI)
  target_compile_options(
    boost_container
    PRIVATE
    $<$<COMPILE_LANGUAGE:CXX>:-frtti>
  )
endif ()

if (EMSCRIPTEN)
  # HACK(strager): In STANDALONE_WASM mode, emscripten generates calls to
  # clock_time_get, originating from Boost. As of Node.js version v12.20.0, such
  # calls require Node to be run with --experimental-wasm-bigint. Without this
  # flag, clock_time_get calls fail with the following message:
  #
  # > Error: TypeError: wasm function signature contains illegal type
  #
  # Our Visual Studio Code plugin cannot enable this Node.js flag itself. Work
  # around emscripten's code gen by avoiding the call to clock_time_get in
  # Boost's dlmalloc.
  target_compile_definitions(boost_container PRIVATE LACKS_TIME_H)
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
