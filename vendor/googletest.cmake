# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

set(BUILD_GMOCK TRUE CACHE INTERNAL "")
set(INSTALL_GTEST FALSE CACHE INTERNAL "")
set(gmock_build_tests FALSE CACHE INTERNAL "")
set(gtest_build_samples FALSE CACHE INTERNAL "")
set(gtest_build_tests FALSE CACHE INTERNAL "")
set(gtest_force_shared_crt TRUE CACHE INTERNAL "")

add_subdirectory(googletest)

# HACK(strager): googletest forces C++11. This causes problems with their
# std::string_view feature detection. Force C++17.
foreach (TARGET gmock gmock_main gtest gtest_main)
  target_compile_features("${TARGET}" PUBLIC cxx_std_17)
endforeach ()

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
