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

set(JSONCPP_WITH_CMAKE_PACKAGE FALSE CACHE INTERNAL "")
set(JSONCPP_WITH_EXAMPLE FALSE CACHE INTERNAL "")
set(JSONCPP_WITH_PKGCONFIG_SUPPORT FALSE CACHE INTERNAL "")
set(JSONCPP_WITH_POST_BUILD_UNITTEST FALSE CACHE INTERNAL "")
set(JSONCPP_WITH_STRICT_ISO FALSE CACHE INTERNAL "")
set(JSONCPP_WITH_TESTS FALSE CACHE INTERNAL "")
set(JSONCPP_WITH_WARNING_AS_ERROR FALSE CACHE INTERNAL "")

# HACK(strager): Prevent JsonCpp from setting these variables.
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "" CACHE PATH "")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "" CACHE PATH "")
set(CMAKE_PDB_OUTPUT_DIRECTORY     "" CACHE PATH "")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "" CACHE PATH "")

add_subdirectory(jsoncpp EXCLUDE_FROM_ALL)
target_compile_definitions(jsoncpp_lib PUBLIC JSON_USE_EXCEPTION=0)
quick_lint_js_add_warning_options_if_supported(
  jsoncpp_lib
  PUBLIC
  -Wno-deprecated-volatile
)
