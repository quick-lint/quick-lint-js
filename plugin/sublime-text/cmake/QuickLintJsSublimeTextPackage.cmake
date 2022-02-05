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

function (quick_lint_js_sublime_text_get_package_version PACKAGE_VERSION)
  string(REPLACE "." "_" VERSION "${PROJECT_VERSION}")
  string(PREPEND VERSION "v")
  set(PACKAGE_VERSION "${VERSION}" PARENT_SCOPE)
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
  set(PACKAGE_DISTRIBUTION "${PROJECT_NAME}")
  quick_lint_js_sublime_text_get_package_version(PACKAGE_VERSION)
  set(PACKAGE_TAG "st${QUICK_LINT_JS_SUBLIME_TEXT_VERSION}")
  quick_lint_js_sublime_text_get_package_platform(PACKAGE_PLATFORM)
  set(
    PACKAGE_FILENAME
    "${PACKAGE_DISTRIBUTION}-${PACKAGE_VERSION}-${PACKAGE_TAG}-${PACKAGE_PLATFORM}"
  )
endfunction ()

function (quick_lint_js_sublime_text_get_package_pathname PACKAGE_PATHNAME)
  quick_lint_js_sublime_text_get_package_filename(PACKAGE_FILENAME)
  set(PACKAGE_PATHNAME "${CMAKE_CURRENT_BINARY_DIR}/${PACKAGE_FILENAME}")
endfunction ()

function (quick_lint_js_sublime_text_get_package_files PACKAGE_FILES)
  set(
    SUBLIME_TEXT_PACKAGE_FILES
    $<TARGET_FILE:quick-lint-js-lib>
    "${CMAKE_CURRENT_SOURCE_DIR}/.no-sublime-package"
    "${CMAKE_CURRENT_SOURCE_DIR}/interface.py"
    "${CMAKE_CURRENT_SOURCE_DIR}/extension.py"
    "${CMAKE_CURRENT_SOURCE_DIR}/interface.py"
  )
  # TODO(cahian): Dynamicaly get all necessary dependencies from quick-lint-js-lib
  # ```
  #   get_target_property(OUT quick-lint-js-lib LINK_LIBRARIES)
  #   message(STATUS ${OUT})
  # ```
  if (WIN32)
    list(APPEND SUBLIME_TEXT_PACKAGE_FILES $<TARGET_FILE:boost>)
  endif ()
endfunction ()

# if(NOT CMAKE_PROPERTY_LIST)
#     execute_process(COMMAND cmake --help-property-list OUTPUT_VARIABLE CMAKE_PROPERTY_LIST)
#     
#     # Convert command output into a CMake list
#     string(REGEX REPLACE ";" "\\\\;" CMAKE_PROPERTY_LIST "${CMAKE_PROPERTY_LIST}")
#     string(REGEX REPLACE "\n" ";" CMAKE_PROPERTY_LIST "${CMAKE_PROPERTY_LIST}")
# endif()
#     
# function(print_properties)
#     message("CMAKE_PROPERTY_LIST = ${CMAKE_PROPERTY_LIST}")
# endfunction()
#     
# function(print_target_properties target)
#     if(NOT TARGET ${target})
#       message(STATUS "There is no target named '${target}'")
#       return()
#     endif()
# 
#     foreach(property ${CMAKE_PROPERTY_LIST})
#         string(REPLACE "<CONFIG>" "${CMAKE_BUILD_TYPE}" property ${property})
# 
#         # Fix https://stackoverflow.com/questions/32197663/how-can-i-remove-the-the-location-property-may-not-be-read-from-target-error-i
#         if(property STREQUAL "LOCATION" OR property MATCHES "^LOCATION_" OR property MATCHES "_LOCATION$")
#             continue()
#         endif()
# 
#         get_property(was_set TARGET ${target} PROPERTY ${property} SET)
#         if(was_set)
#             get_target_property(value ${target} ${property})
#             message("${target} ${property} = ${value}")
#         endif()
#     endforeach()
# endfunction()

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
