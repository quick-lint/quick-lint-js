# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

cmake_minimum_required(VERSION 3.10)

if (QUICK_LINT_JS_SUBLIME_TEXT_3 OR
    QUICK_LINT_JS_SUBLIME_TEXT_4)
  set(QUICK_LINT_JS_SUBLIME_TEXT TRUE)
endif ()

# This code snippet is responsible for allowing CMake to **run again**
# without errors if you don't declare `BUILD_SHARED_LIBS=ON` or
# `CMAKE_POSITION_INDEPENDENT_CODE=ON` on the command line.
if (CACHED_BUILD_SHARED_LIBS)
  set(BUILD_SHARED_LIBS ON)
endif ()
if (CACHED_CMAKE_POSITION_INDEPENDENT_CODE)
  set(CMAKE_POSITION_INDEPENDENT_CODE ON)
endif ()
if (BUILD_SHARED_LIBS)
  set(CACHED_BUILD_SHARED_LIBS ON CACHE BOOL ON)
endif ()
if (CMAKE_POSITION_INDEPENDENT_CODE)
  set(CACHED_CMAKE_POSITION_INDEPENDENT_CODE ON CACHE BOOL ON)
endif ()

if (QUICK_LINT_JS_SUBLIME_TEXT AND
    BUILD_SHARED_LIBS AND
    CMAKE_POSITION_INDEPENDENT_CODE)
  # 0.2.0 -> 0_2_0
  string(
    REPLACE
    "." "_"
    SUBLIME_TEXT_PROJECT_VERSION
    "${PROJECT_VERSION}"
  )
  # 0_2_0 -> v0_2_0
  string(
    PREPEND
    SUBLIME_TEXT_PROJECT_VERSION
    "v"
  )

  if (QUICK_LINT_JS_SUBLIME_TEXT_3)
    set(SUBLIME_TEXT_VERSION_TAG "subl3")
    set(SUBLIME_TEXT_VERSION "sublime-text-3")
  elseif (QUICK_LINT_JS_SUBLIME_TEXT_4)
    set(SUBLIME_TEXT_VERSION_TAG "subl4")
    set(SUBLIME_TEXT_VERSION "sublime-text-4")
  endif ()

  set(
    SUBLIME_TEXT_PACKAGE_PLATFORM
    "linux_${CMAKE_SYSTEM_PROCESSOR}"
  )
  set(
    SUBLIME_TEXT_PACKAGE_PATH
    "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-\
${SUBLIME_TEXT_PROJECT_VERSION}-${SUBLIME_TEXT_VERSION_TAG}-\
${SUBLIME_TEXT_PACKAGE_PLATFORM}"
  )
  set(
    SUBLIME_TEXT_PACKAGE_FILES
    $<TARGET_FILE:quick-lint-js-lib>
    "${CMAKE_CURRENT_SOURCE_DIR}/c_api_${SUBLIME_TEXT_VERSION_TAG}.py"
    "${CMAKE_CURRENT_SOURCE_DIR}/plugin_${SUBLIME_TEXT_VERSION_TAG}.py"
    "${CMAKE_CURRENT_SOURCE_DIR}/${SUBLIME_TEXT_VERSION}.python-version"
    "${CMAKE_CURRENT_SOURCE_DIR}/.no-sublime-package"
  )

  add_custom_target(
    quick-lint-js-sublime-text
    COMMAND "${CMAKE_COMMAND}" -E make_directory
    "${SUBLIME_TEXT_PACKAGE_PATH}"
    COMMAND "${CMAKE_COMMAND}" -E copy
    ${SUBLIME_TEXT_PACKAGE_FILES}
    "${SUBLIME_TEXT_PACKAGE_PATH}"
    COMMAND "${CMAKE_COMMAND}" -E rename
    "${SUBLIME_TEXT_PACKAGE_PATH}/c_api_${SUBLIME_TEXT_VERSION_TAG}.py"
    "${SUBLIME_TEXT_PACKAGE_PATH}/c_api.py"
    COMMAND "${CMAKE_COMMAND}" -E rename
    "${SUBLIME_TEXT_PACKAGE_PATH}/plugin_${SUBLIME_TEXT_VERSION_TAG}.py"
    "${SUBLIME_TEXT_PACKAGE_PATH}/plugin.py"
    COMMAND "${CMAKE_COMMAND}" -E rename
    "${SUBLIME_TEXT_PACKAGE_PATH}/${SUBLIME_TEXT_VERSION}.python-version"
    "${SUBLIME_TEXT_PACKAGE_PATH}/.python-version"
  )
  add_dependencies(quick-lint-js-sublime-text quick-lint-js-lib)

  if (QUICK_LINT_JS_SUBLIME_TEXT_3)
    set(
      SUBLIME_TEXT_PACKAGE_DESTINATION
      "$ENV{HOME}/.config/sublime-text-3/Packages"
    )
  elseif (QUICK_LINT_JS_SUBLIME_TEXT_4)
    if (IS_DIRECTORY "$ENV{HOME}/.config/sublime-text/Packages")
      set(
        SUBLIME_TEXT_PACKAGE_DESTINATION
        "$ENV{HOME}/.config/sublime-text/Packages"
      )
    elseif (IS_DIRECTORY "$ENV{HOME}/.config/sublime-text-3/Packages")
      set(
        SUBLIME_TEXT_PACKAGE_DESTINATION
        "$ENV{HOME}/.config/sublime-text-3/Packages"
      )
    else ()
      set(
        SUBLIME_TEXT_PACKAGE_DESTINATION
        "$ENV{HOME}/.config/sublime-text/Packages"
      )
    endif ()
  endif ()
  install(
    FILES "${SUBLIME_TEXT_PACKAGE_PATH}"
    COMPONENT sublime-text
    EXCLUDE_FROM_ALL
    DESTINATION "${SUBLIME_TEXT_PACKAGE_DESTINATION}"
  )
elseif (QUICK_LINT_JS_SUBLIME_TEXT)
  message(
    FATAL_ERROR
    "You need to set BUILD_SHARED_LIBS=ON and CMAKE_POSITION_INDEPENDENT_CODE=ON \
to build the Sublime Text plugin."
  )
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
