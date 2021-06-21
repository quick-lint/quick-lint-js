# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

set(BENCHMARK_DOWNLOAD_DEPENDENCIES FALSE CACHE INTERNAL "")
set(BENCHMARK_ENABLE_ASSEMBLY_TEST FALSE CACHE INTERNAL "")
set(BENCHMARK_ENABLE_GTEST_TESTS FALSE CACHE INTERNAL "")
set(BENCHMARK_ENABLE_INSTALL FALSE CACHE INTERNAL "")
set(BENCHMARK_ENABLE_LTO FALSE CACHE INTERNAL "")
set(BENCHMARK_ENABLE_TESTING FALSE CACHE INTERNAL "")
set(BENCHMARK_USE_LIBCXX FALSE CACHE INTERNAL "")

if (MSVC)
  # HACK(strager): Disable MSVC warnings when compiling benchmark and
  # benchmark_main:
  # * D9025: overriding '/EHs' with '/EHs-'
  set(BENCHMARK_ENABLE_EXCEPTIONS TRUE CACHE INTERNAL "")
else ()
  set(BENCHMARK_ENABLE_EXCEPTIONS FALSE CACHE INTERNAL "")
endif ()

add_subdirectory(benchmark)

# HACK(strager): Avoid 'function can be marked override' warnings
# (-Wsuggest-override) when including <benchmark/benchmark.h>.
get_property(
  BENCHMARK_INCLUDE_DIRECTORIES
  TARGET benchmark
  PROPERTY INTERFACE_INCLUDE_DIRECTORIES
)
set_property(
  TARGET benchmark
  APPEND PROPERTY
  INTERFACE_SYSTEM_INCLUDE_DIRECTORIES
  "${BENCHMARK_INCLUDE_DIRECTORIES}"
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
