# Copyright (C) 2020  Matthew Glazar
# See end of file for extended copyright information.

cmake_minimum_required(VERSION 3.10)
include(GNUInstallDirs)

add_executable(
  quick-lint-js-generate-error-docs
  generate-error-docs.cpp
)
target_link_libraries(
  quick-lint-js-generate-error-docs
  PRIVATE
  md4c
  quick-lint-js-docs-lib
  quick-lint-js-lib
)

add_library(
  quick-lint-js-docs-lib
  error-documentation.cpp
)
target_include_directories(quick-lint-js-docs-lib PUBLIC .)
target_link_libraries(
  quick-lint-js-docs-lib
  PUBLIC
  quick-lint-js-lib
  PRIVATE
  md4c
)

install(
  FILES quick-lint-js.1
  DESTINATION "${CMAKE_INSTALL_MANDIR}/man1"
)

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
