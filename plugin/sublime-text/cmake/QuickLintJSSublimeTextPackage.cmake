# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

function (quick_lint_js_sublime_text_add_colored_compiler_output)
  string(TOLOWER "${CMAKE_CXX_COMPILER_ID}" CMAKE_CXX_COMPILER_ID_LOWER)
  if ("${CMAKE_CXX_COMPILER_ID_LOWER}" STREQUAL "gnu")
    add_compile_options(-fdiagnostics-color=always)
  elseif ("${CMAKE_CXX_COMPILER_ID_LOWER}" STREQUAL "clang")
    add_compile_options(-fcolor-diagnostics)
  endif ()
endfunction ()

function (quick_lint_js_sublime_text_set_compiler_options)
  set(BUILD_SHARED_LIBS ON)
  set(CMAKE_POSITION_INDEPENDENT_CODE ON)
  if (WIN32)
    set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)
  endif ()
endfunction ()

function (quick_lint_js_sublime_text_get_package_distribution PACKAGE_DISTRIBUTION)
  set(PACKAGE_DISTRIBUTION "${PROJECT_NAME}")
endfunction ()

function (quick_lint_js_sublime_text_get_package_version PACKAGE_VERSION)
  string(REPLACE "." "_" VERSION "${PROJECT_VERSION}")
  string(PREPEND VERSION "v")
  set(PACKAGE_VERSION "${VERSION}" PARENT_SCOPE)
endfunction ()

function (quick_lint_js_sublime_text_get_package_tag PACKAGE_TAG)
  set(PACKAGE_TAG "st${QUICK_LINT_JS_SUBLIME_TEXT_VERSION}")
endfunction ()

function (quick_lint_js_sublime_text_get_package_platform PACKAGE_PLATFORM)
  if (WIN32)
    set(PACKAGE_NAME "windows")
  elseif (APPLE)
    set(PACKAGE_NAME "macosx")
  else ()
    string(TOLOWER PACKAGE_NAME "${CMAKE_SYSTEM_NAME}")
  endif ()
  string(TOLOWER PACKAGE_PROCESSOR "${CMAKE_SYSTEM_PROCESSOR}")
  set(PACKAGE_PLATFORM "${PACKAGE_NAME}_${PACKAGE_PROCESSOR}" PARENT_SCOPE)
endfunction ()

function (quick_lint_js_sublime_text_get_package_filename PACKAGE_FILENAME)
  quick_lint_js_sublime_text_get_package_distribution(PACKAGE_DISTRIBUTION)
  quick_lint_js_sublime_text_get_package_version(PACKAGE_VERSION)
  quick_lint_js_sublime_text_get_package_tag(PACKAGE_TAG)
  quick_lint_js_sublime_text_get_package_platform(PACKAGE_PLATFORM)
  set(PACKAGE_FILENAME "${PACKAGE_DISTRIBUTION}-${PACKAGE_VERSION}-${PACKAGE_TAG}-${PACKAGE_PLATFORM}")
endfunction ()

function (quick_lint_js_sublime_text_get_package_pathname PACKAGE_PATHNAME)
  quick_lint_js_sublime_text_get_package_filename(PACKAGE_FILENAME)
  set(PACKAGE_PATHNAME "${CMAKE_CURRENT_BINARY_DIR}/${PACKAGE_FILENAME}")
endfunction ()

function (quick_lint_js_sublime_text_get_package_files PACKAGE_FILES)
  set(
    PACKAGE_FILES
    $<TARGET_FILE:quick-lint-js-sublime-text>
    "${CMAKE_CURRENT_SOURCE_DIR}/.no-sublime-package"
    "${CMAKE_CURRENT_SOURCE_DIR}/extension.py"
    "${CMAKE_CURRENT_SOURCE_DIR}/interface.py"
  )
  if (WIN32)
    if (CMAKE_VERSION VERSION_GREATER_EQUAL "3.21")
      list(
        APPEND
        SUBLIME_TEXT_PACKAGE_FILES
        $<TARGET_RUNTIME_DLLS:quick-lint-js-sublime-text>
      )
    else ()
      list(APPEND SUBLIME_TEXT_PACKAGE_FILES $<TARGET_FILE:boost>)
      message(WARNING "Please use CMake version 3.21 or higher. Maybe the plugin will not work on Windows due to the lack of DLL files that could not be automatically detected.")
    endif ()
  endif ()
endfunction ()

function (quick_lint_js_sublime_text_get_package_destination PACKAGE_DESTINATION)
  if (WIN32)
    string(REPLACE "\\" "/" APPDATA "$ENV{APPDATA}")
    set(SUBLIME_TEXT_3_PACKAGE_LOCATION "${APPDATA}/Sublime Text 3/Packages")
    set(SUBLIME_TEXT_PACKAGE_LOCATION "${APPDATA}/Sublime Text/Packages")
  elseif (APPLE)
    set(HOME "$ENV{HOME}")
    set(SUBLIME_TEXT_3_PACKAGE_LOCATION "${HOME}/Library/Application Support/Sublime Text 3/Packages")
    set(SUBLIME_TEXT_PACKAGE_LOCATION "${HOME}/Library/Application Support/Sublime Text/Packages")
  else ()
    set(HOME "$ENV{HOME}")
    set(SUBLIME_TEXT_3_PACKAGE_LOCATION "${HOME}/.config/sublime-text-3/Packages")
    set(SUBLIME_TEXT_PACKAGE_LOCATION "${HOME}/.config/sublime-text/Packages")
  endif ()

  if (QUICK_LINT_JS_SUBLIME_TEXT_VERSION EQUAL 3)
    set(PACKAGE_DESTINATION "${SUBLIME_TEXT_3_PACKAGE_LOCATION}")
  elseif (QUICK_LINT_JS_SUBLIME_TEXT_VERSION GREATER 3)
    if (IS_DIRECTORY SUBLIME_TEXT_4_PACKAGE_LOCATION)
      set(PACKAGE_DESTINATION "${SUBLIME_TEXT_PACKAGE_LOCATION}")
    elseif (IS_DIRECTORY SUBLIME_TEXT_3_PACKAGE_LOCATION)
      set(PACKAGE_DESTINATION "${SUBLIME_TEXT_3_PACKAGE_LOCATION}")
    else ()
      set(PACKAGE_DESTINATION "${SUBLIME_TEXT_PACKAGE_LOCATION}")
    endif ()
  endif ()
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
