# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

include(CheckCXXCompilerFlag)

add_library(
  boost
  "${CMAKE_CURRENT_LIST_DIR}/boost/libs/container/src/alloc_lib.c"
  "${CMAKE_CURRENT_LIST_DIR}/boost/libs/container/src/dlmalloc.cpp"
  "${CMAKE_CURRENT_LIST_DIR}/boost/libs/container/src/global_resource.cpp"
  "${CMAKE_CURRENT_LIST_DIR}/boost/libs/container/src/monotonic_buffer_resource.cpp"
  "${CMAKE_CURRENT_LIST_DIR}/boost/libs/container/src/pool_resource.cpp"
  "${CMAKE_CURRENT_LIST_DIR}/boost/libs/container/src/synchronized_pool_resource.cpp"
  "${CMAKE_CURRENT_LIST_DIR}/boost/libs/container/src/unsynchronized_pool_resource.cpp"
)
# NOTE(strager): SYSTEM disable undesirable warnings in Boost header files.
target_include_directories(boost SYSTEM PUBLIC "${CMAKE_CURRENT_LIST_DIR}/boost")
target_compile_definitions(
  boost
  PUBLIC
  BOOST_ALL_NO_LIB
  BOOST_CONTAINER_NO_LIB
  BOOST_JSON_STANDALONE
  BOOST_JSON_USE_BOOST_PMR
  BOOST_LEAF_NO_EXCEPTIONS
  BOOST_NO_EXCEPTIONS
)
# Disable undesirable warnings in headers and source files.
quick_lint_js_add_warning_options_if_supported(
  boost
  PRIVATE
  -Wno-null-pointer-arithmetic
)

# Allow the entire project to be compiled with -fno-rtti. Boost uses
# dynamic_cast which requires RTTI, so forcefully enable RTTI for Boost.
check_cxx_compiler_flag(-frtti QUICK_LINT_JS_HAVE_FRTTI)
if (QUICK_LINT_JS_HAVE_FRTTI)
  target_compile_options(
    boost
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
  target_compile_definitions(boost PRIVATE LACKS_TIME_H)
endif ()

# Keep boost_json as a separate library so we can use it only in tests (and not
# compile and link it into production executables).
add_library(boost_json STATIC "${CMAKE_CURRENT_LIST_DIR}/boost-json.cpp")
target_link_libraries(boost_json PUBLIC boost)

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
