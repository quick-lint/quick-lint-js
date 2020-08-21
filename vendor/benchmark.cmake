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

set(BENCHMARK_DOWNLOAD_DEPENDENCIES FALSE CACHE INTERNAL "")
set(BENCHMARK_ENABLE_ASSEMBLY_TEST FALSE CACHE INTERNAL "")
set(BENCHMARK_ENABLE_EXCEPTIONS FALSE CACHE INTERNAL "")
set(BENCHMARK_ENABLE_GTEST_TESTS FALSE CACHE INTERNAL "")
set(BENCHMARK_ENABLE_INSTALL FALSE CACHE INTERNAL "")
set(BENCHMARK_ENABLE_LTO FALSE CACHE INTERNAL "")
set(BENCHMARK_ENABLE_TESTING FALSE CACHE INTERNAL "")
set(BENCHMARK_USE_LIBCXX FALSE CACHE INTERNAL "")

add_subdirectory(benchmark)
