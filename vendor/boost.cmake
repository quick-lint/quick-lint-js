# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

if (QUICK_LINT_JS_USE_BUNDLED_BOOST)
  include(CheckCXXCompilerFlag)

  add_library(boost INTERFACE)
  # NOTE(strager): SYSTEM disable undesirable warnings in Boost header files.
  target_include_directories(boost SYSTEM INTERFACE "${CMAKE_CURRENT_LIST_DIR}/boost")
  target_compile_definitions(
    boost
    INTERFACE
    BOOST_ALL_NO_LIB
    BOOST_CONTAINER_NO_LIB
    BOOST_EXCEPTION_MINI_BOOST
    BOOST_NO_EXCEPTIONS
  )
else ()
  find_package(Boost REQUIRED COMPONENTS container)
  add_library(boost INTERFACE)
  target_link_libraries(boost INTERFACE Boost::boost Boost::container)
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
