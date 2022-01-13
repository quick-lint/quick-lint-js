# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

function (qljs_sublime_text_add_colored_compiler_output)
  string(TOLOWER "${CMAKE_CXX_COMPILER_ID}" CMAKE_CXX_COMPILER_ID_LOWER)
  if ("${CMAKE_CXX_COMPILER_ID_LOWER}" STREQUAL "gnu")
    add_compile_options(-fdiagnostics-color=always)
  elseif ("${CMAKE_CXX_COMPILER_ID_LOWER}" STREQUAL "clang")
    add_compile_options(-fcolor-diagnostics)
  endif ()
endfunction ()

function (qljs_sublime_text_set_compiler_options)
  set(BUILD_SHARED_LIBS ON)
  set(CMAKE_POSITION_INDEPENDENT_CODE ON)
  if (WIN32)
    set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)
  endif ()
endfunction ()

function (qljs_sublime_text_get_package_version PACKAGE_VERSION)
  string(REPLACE "." "_" VERSION "${PROJECT_VERSION}")
  string(PREPEND VERSION "v")
  set(PACKAGE_VERSION "${VERSION}" PARENT_SCOPE)
endfunction ()

function (qljs_sublime_text_get_package_platform PACKAGE_PLATFORM)
  if (WIN32)
    set(NAME "windows")
  elseif (APPLE)
    set(NAME "macosx")
  else ()
    string(TOLOWER NAME "${CMAKE_SYSTEM_NAME}")
  endif ()
  string(TOLOWER PROCESSOR "${CMAKE_SYSTEM_PROCESSOR}")
  set(PACKAGE_PLATFORM "${NAME}_${PROCESSOR}" PARENT_SCOPE)
endfunction ()

function (qljs_sublime_text_get_package_filename PACKAGE_FILENAME)
  qljs_sublime_text_get_package_version(VERSION)
  qljs_sublime_text_get_package_platform(PLATFORM)
  set(DISTRIBUTION "${PROJECT_NAME}")
  set(SUBLIME_TEXT_TAG "st${QUICK_LINT_JS_SUBLIME_TEXT_VERSION}")
  set(PACKAGE_FILENAME "${DISTRIBUTION}-${VERSION}-${SUBLIME_TEXT_TAG}-${PLATFORM}")
endfunction ()

function (qljs_sublime_text_get_package_pathname PACKAGE_PATHNAME)
  qljs_sublime_text_get_package_filename(FILENAME)
  set(PACKAGE_PATHNAME "${CMAKE_CURRENT_BINARY_DIR}/${FILENAME}")
endfunction ()

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
