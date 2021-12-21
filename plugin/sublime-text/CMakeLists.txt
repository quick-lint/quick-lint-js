# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

cmake_minimum_required(VERSION 3.10)

## QUICK_LINT_JS_SUBLIME_TEXT ##################################################
if (QUICK_LINT_JS_SUBLIME_TEXT_3 OR QUICK_LINT_JS_SUBLIME_TEXT_4)
  set(QUICK_LINT_JS_SUBLIME_TEXT TRUE)
endif ()

# This code part is responsible for allowing CMake to RUN AGAIN
# without errors if you don't declare BUILD_SHARED_LIBS=ON or
# CMAKE_POSITION_INDEPENDENT_CODE=ON on the command line.
#
# For example, it allows the Ninja build system to re-run this
# CMake file without problems.
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
if (WIN32)
  if (CACHED_CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS)
    set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)
  endif ()
  if (CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS)
    set(CACHED_CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON CACHE BOOL ON)
  endif ()
endif ()

if (QUICK_LINT_JS_SUBLIME_TEXT)
  #### FATAL_ERROR ####
  if (NOT (BUILD_SHARED_LIBS AND CMAKE_POSITION_INDEPENDENT_CODE))
    if (WIN32 AND NOT CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS)
      message(
        FATAL_ERROR
        "You need to set BUILD_SHARED_LIBS=ON, CMAKE_POSITION_INDEPENDENT_CODE=ON, \
and CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS=ON to build the Sublime Text plugin."
      )
    else ()
      message(
        FATAL_ERROR
        "You need to set BUILD_SHARED_LIBS=ON and CMAKE_POSITION_INDEPENDENT_CODE=ON \
to build the Sublime Text plugin.")
    endif ()
  endif ()

  #### SUBLIME_TEXT_PROJECT_VERSION ####
  # 0.2.0 -> 0_2_0
  string(REPLACE "." "_" SUBLIME_TEXT_PROJECT_VERSION "${PROJECT_VERSION}")
  # 0_2_0 -> v0_2_0
  string(PREPEND SUBLIME_TEXT_PROJECT_VERSION "v")

  #### SUBLIME_TEXT_VERSION_TAG ####
  if (QUICK_LINT_JS_SUBLIME_TEXT_3)
    set(SUBLIME_TEXT_VERSION_TAG "st3")
  elseif (QUICK_LINT_JS_SUBLIME_TEXT_4)
    set(SUBLIME_TEXT_VERSION_TAG "st4")
  endif ()

  #### SUBLIME_TEXT_PACKAGE_PLATFORM ####
  if (WIN32)
    set(SUBLIME_TEXT_PACKAGE_SYSTEM_NAME "windows")
  elseif (APPLE)
    set(SUBLIME_TEXT_PACKAGE_SYSTEM_NAME "macosx")
  elseif (UNIX AND "${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
    set(SUBLIME_TEXT_PACKAGE_SYSTEM_NAME "linux")
  else ()
    message(
      FATAL_ERROR
      "You can only build the Sublime Text plugin on Windows, macOS, and Linux."
    )
  endif ()
  string(TOLOWER "${CMAKE_SYSTEM_PROCESSOR}" SUBLIME_TEXT_PACKAGE_SYSTEM_PROCESSOR)
  set(
    SUBLIME_TEXT_PACKAGE_PLATFORM
    "${SUBLIME_TEXT_PACKAGE_SYSTEM_NAME}_${SUBLIME_TEXT_PACKAGE_SYSTEM_PROCESSOR}"
  )

  #### SUBLIME_TEXT_PACKAGE_PATH ####
  set(
    SUBLIME_TEXT_PACKAGE_PATH
    "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-${SUBLIME_TEXT_PROJECT_VERSION}-\
${SUBLIME_TEXT_VERSION_TAG}-${SUBLIME_TEXT_PACKAGE_PLATFORM}"
  )

  #### SUBLIME_TEXT_PACKAGE_FILES ####
  set(
    SUBLIME_TEXT_PACKAGE_FILES
    $<TARGET_FILE:quick-lint-js-lib>
    "${CMAKE_CURRENT_SOURCE_DIR}/${SUBLIME_TEXT_VERSION_TAG}/c_api.py"
    "${CMAKE_CURRENT_SOURCE_DIR}/${SUBLIME_TEXT_VERSION_TAG}/plugin.py"
    "${CMAKE_CURRENT_SOURCE_DIR}/${SUBLIME_TEXT_VERSION_TAG}/.python-version"
    "${CMAKE_CURRENT_SOURCE_DIR}/.no-sublime-package"
  )
  if (WIN32)
    # Add DLLs that quick-lint-js-lib depends on.
    list(APPEND SUBLIME_TEXT_PACKAGE_FILES $<TARGET_FILE:boost>)
  endif ()

  #### SUBLIME_TEXT_PACKAGE_DESTINATION ####
  if (WIN32)
    string(REPLACE "\\" "/" APPDATA "$ENV{APPDATA}")
    set(SUBLIME_TEXT_3_PACKAGE_LOCATION "${APPDATA}/Sublime Text 3/Packages")
    set(SUBLIME_TEXT_4_PACKAGE_LOCATION "${APPDATA}/Sublime Text/Packages")
  elseif (APPLE)
    set(HOME "$ENV{HOME}")
    set(SUBLIME_TEXT_3_PACKAGE_LOCATION "${HOME}/Library/Application Support/Sublime Text 3/Packages")
    set(SUBLIME_TEXT_4_PACKAGE_LOCATION "${HOME}/Library/Application Support/Sublime Text/Packages")
  else ()
    set(HOME "$ENV{HOME}")
    set(SUBLIME_TEXT_3_PACKAGE_LOCATION "${HOME}/.config/sublime-text-3/Packages")
    set(SUBLIME_TEXT_4_PACKAGE_LOCATION "${HOME}/.config/sublime-text/Packages")
  endif ()
  if (QUICK_LINT_JS_SUBLIME_TEXT_3)
    set(SUBLIME_TEXT_PACKAGE_DESTINATION "${SUBLIME_TEXT_3_PACKAGE_LOCATION}")
  elseif (QUICK_LINT_JS_SUBLIME_TEXT_4)
    if (IS_DIRECTORY SUBLIME_TEXT_4_PACKAGE_LOCATION)
      set(SUBLIME_TEXT_PACKAGE_DESTINATION "${SUBLIME_TEXT_4_PACKAGE_LOCATION}")
    elseif (IS_DIRECTORY SUBLIME_TEXT_3_PACKAGE_LOCATION)
      set(SUBLIME_TEXT_PACKAGE_DESTINATION "${SUBLIME_TEXT_3_PACKAGE_LOCATION}")
    else ()
      set(SUBLIME_TEXT_PACKAGE_DESTINATION "${SUBLIME_TEXT_4_PACKAGE_LOCATION}")
    endif ()
  endif ()

  #### BUILD ####
  add_custom_target(
    quick-lint-js-sublime-text
    COMMAND "${CMAKE_COMMAND}" -E make_directory
    "${SUBLIME_TEXT_PACKAGE_PATH}"
    COMMAND "${CMAKE_COMMAND}" -E copy
    ${SUBLIME_TEXT_PACKAGE_FILES}
    "${SUBLIME_TEXT_PACKAGE_PATH}"
    VERBATIM
  )
  add_dependencies(quick-lint-js-sublime-text quick-lint-js-lib)

  #### INSTALL ####
  install(
    DIRECTORY "${SUBLIME_TEXT_PACKAGE_PATH}"
    COMPONENT sublime-text
    EXCLUDE_FROM_ALL
    DESTINATION "${SUBLIME_TEXT_PACKAGE_DESTINATION}"
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
