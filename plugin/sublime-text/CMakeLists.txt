# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

cmake_minimum_required(VERSION 3.21)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
include(QuickLintJSCopyright)
include(QuickLintJSSublimeTextPackage)
include(QuickLintJSTarget)

if (QUICK_LINT_JS_SUBLIME_TEXT_VERSION GREATER_EQUAL 3)
  quick_lint_js_sublime_text_add_colored_compiler_output()
  quick_lint_js_sublime_text_set_compiler_options()
  quick_lint_js_sublime_text_get_package_pathname(SUBLIME_TEXT_PACKAGE_PATHNAME)
  quick_lint_js_sublime_text_get_package_files(SUBLIME_TEXT_PACKAGE_FILES)

  quick_lint_js_add_library(
    quick-lint-js-sublime-text
    MODULE  # You cannot link to a MODULE, but you can link to a SHARED
    sublime-text-interface.cpp
    sublime-text-location.cpp
    quick-lint-js/sublime-text-have.h
    quick-lint-js/sublime-text-interface.h
    quick-lint-js/sublime-text-location.h
  )
  target_include_directories(quick-lint-js-sublime-text PRIVATE .)
  target_link_libraries(quick-lint-js-sublime-text PRIVATE quick-lint-js-lib)

  # install()
  # quick_lint_js_collect_copyright(
  #   quick-lint-js-sublime-text-licenses
  #   TARGET quick-lint-js-sublime-text
  #   OUTPUT_FILE quick-lint-js-sublime-text-licenses.txt
  # )
else ()
  # TODO(cahian): Add a message error here
endif ()

#### SUBLIME_TEXT_PACKAGE_FILES ####
# set(
#   SUBLIME_TEXT_PACKAGE_FILES
#   $<TARGET_FILE:quick-lint-js-lib>
#   "${CMAKE_CURRENT_SOURCE_DIR}/interface.py"
#   "${CMAKE_CURRENT_SOURCE_DIR}/extension.py"
#   "${CMAKE_CURRENT_SOURCE_DIR}/.python-version"
#   "${CMAKE_CURRENT_SOURCE_DIR}/.no-sublime-package"
# )
# if (WIN32)
#   # Add DLLs that quick-lint-js-lib depends on.
#   list(APPEND SUBLIME_TEXT_PACKAGE_FILES $<TARGET_FILE:boost>)
# endif ()

# #### SUBLIME_TEXT_PACKAGE_DESTINATION ####
# if (WIN32)
#   string(REPLACE "\\" "/" APPDATA "$ENV{APPDATA}")
#   set(SUBLIME_TEXT_3_PACKAGE_LOCATION "${APPDATA}/Sublime Text 3/Packages")
#   set(SUBLIME_TEXT_4_PACKAGE_LOCATION "${APPDATA}/Sublime Text/Packages")
# elseif (APPLE)
#   set(HOME "$ENV{HOME}")
#   set(SUBLIME_TEXT_3_PACKAGE_LOCATION "${HOME}/Library/Application Support/Sublime Text 3/Packages")
#   set(SUBLIME_TEXT_4_PACKAGE_LOCATION "${HOME}/Library/Application Support/Sublime Text/Packages")
# else ()
#   set(HOME "$ENV{HOME}")
#   set(SUBLIME_TEXT_3_PACKAGE_LOCATION "${HOME}/.config/sublime-text-3/Packages")
#   set(SUBLIME_TEXT_4_PACKAGE_LOCATION "${HOME}/.config/sublime-text/Packages")
# endif ()
# if (QUICK_LINT_JS_SUBLIME_TEXT_3)
#   set(SUBLIME_TEXT_PACKAGE_DESTINATION "${SUBLIME_TEXT_3_PACKAGE_LOCATION}")
# elseif (QUICK_LINT_JS_SUBLIME_TEXT_4)
#   if (IS_DIRECTORY SUBLIME_TEXT_4_PACKAGE_LOCATION)
#     set(SUBLIME_TEXT_PACKAGE_DESTINATION "${SUBLIME_TEXT_4_PACKAGE_LOCATION}")
#   elseif (IS_DIRECTORY SUBLIME_TEXT_3_PACKAGE_LOCATION)
#     set(SUBLIME_TEXT_PACKAGE_DESTINATION "${SUBLIME_TEXT_3_PACKAGE_LOCATION}")
#   else ()
#     set(SUBLIME_TEXT_PACKAGE_DESTINATION "${SUBLIME_TEXT_4_PACKAGE_LOCATION}")
#   endif ()
# endif ()

# #### BUILD ####
# add_custom_target(
#   quick-lint-js-sublime-text
#   COMMAND "${CMAKE_COMMAND}" -E make_directory
#   "${SUBLIME_TEXT_PACKAGE_PATH}"
#   COMMAND "${CMAKE_COMMAND}" -E copy
#   ${SUBLIME_TEXT_PACKAGE_FILES}
#   "${SUBLIME_TEXT_PACKAGE_PATH}"
#   VERBATIM
# )
# add_dependencies(quick-lint-js-sublime-text quick-lint-js-lib)

# #### INSTALL ####
# install(
#   DIRECTORY "${SUBLIME_TEXT_PACKAGE_PATH}"
#   COMPONENT sublime-text
#   EXCLUDE_FROM_ALL
#   DESTINATION "${SUBLIME_TEXT_PACKAGE_DESTINATION}"
# )


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
