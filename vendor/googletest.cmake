# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

if (QUICK_LINT_JS_USE_BUNDLED_GOOGLE_TEST)
  set(BUILD_GMOCK TRUE CACHE INTERNAL "")
  set(INSTALL_GTEST FALSE CACHE INTERNAL "")
  set(gmock_build_tests FALSE CACHE INTERNAL "")
  set(gtest_build_samples FALSE CACHE INTERNAL "")
  set(gtest_build_tests FALSE CACHE INTERNAL "")
  set(gtest_force_shared_crt TRUE CACHE INTERNAL "")

  add_subdirectory("${CMAKE_CURRENT_LIST_DIR}/googletest")

  # HACK(strager): googletest forces C++11. This causes problems with their
  # std::string_view feature detection. Force C++17.
  foreach (TARGET gmock gmock_main gtest gtest_main)
    target_compile_features("${TARGET}" PUBLIC cxx_std_17)
  endforeach ()
else ()
  find_package(PkgConfig REQUIRED)

  pkg_search_module(GMOCK REQUIRED gmock)
  add_library(gmock INTERFACE)
  target_link_libraries(gmock INTERFACE ${GMOCK_LDFLAGS} ${GMOCK_LIBRARIES})
  target_compile_options(gmock INTERFACE ${GMOCK_CFLAGS})

  pkg_search_module(GMOCK_MAIN REQUIRED gmock_main)
  add_library(gmock_main INTERFACE)
  target_link_libraries(gmock_main INTERFACE ${GMOCK_MAIN_LDFLAGS} ${GMOCK_MAIN_LIBRARIES})
  target_compile_options(gmock_main INTERFACE ${GMOCK_MAIN_CFLAGS})

  pkg_search_module(GTEST REQUIRED gtest)
  add_library(gtest INTERFACE)
  target_link_libraries(gtest INTERFACE ${GTEST_LDFLAGS} ${GTEST_LIBRARIES})
  target_compile_options(gtest INTERFACE ${GTEST_CFLAGS})

  pkg_search_module(GTEST_MAIN REQUIRED gtest_main)
  add_library(gtest_main INTERFACE)
  target_link_libraries(gtest_main INTERFACE ${GTEST_MAIN_LDFLAGS} ${GTEST_MAIN_LIBRARIES})
  target_compile_options(gtest_main INTERFACE ${GTEST_MAIN_CFLAGS})
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
