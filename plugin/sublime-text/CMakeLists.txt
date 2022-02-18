# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

cmake_minimum_required(VERSION 3.10)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
include(QuickLintJSCopyright)
include(QuickLintJSSublimeTextPackage)
include(QuickLintJSTarget)

if (QUICK_LINT_JS_SUBLIME_TEXT_VERSION GREATER_EQUAL 3)
  quick_lint_js_sublime_text_add_colored_compiler_output()
  quick_lint_js_sublime_text_set_compiler_definitions()
  quick_lint_js_sublime_text_set_compiler_options()

  # You cannot link to a MODULE (only load dynamicaly), but you can link to a SHARED.
  quick_lint_js_add_library(
    quick-lint-js-sublime-text
    MODULE  
    sublime-text-interface.cpp
    sublime-text-location.cpp
    quick-lint-js/sublime-text-interface.h
    quick-lint-js/sublime-text-location.h
  )
  quick_lint_js_collect_copyright(
    quick-lint-js-sublime-text-licenses
    TARGET quick-lint-js-sublime-text
    OUTPUT_FILE quick-lint-js-sublime-text-licenses.txt
  )

  target_include_directories(quick-lint-js-sublime-text PRIVATE .)
  target_link_libraries(quick-lint-js-sublime-text PRIVATE quick-lint-js-lib)

  install(
    SCRIPT "cmake/QuickLintJSSublimeTextInstall.cmake"
    COMPONENT sublime-text
    EXCLUDE_FROM_ALL
  )
elseif (DEFINED QUICK_LINT_JS_SUBLIME_TEXT_VERSION)
  message(FATAL_ERROR "Sublime Text version not supported.")
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
