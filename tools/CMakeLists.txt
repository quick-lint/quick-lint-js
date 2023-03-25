# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

cmake_minimum_required(VERSION 3.10)
include(QuickLintJSTarget)

if (NOT EMSCRIPTEN) # We don't support tools targeting Emscripten.
  quick_lint_js_add_executable(quick-lint-js-analyze-trace analyze-trace.cpp)
  target_link_libraries(
    quick-lint-js-analyze-trace
    PRIVATE
    quick-lint-js-lib
  )
endif ()

set(QUICK_LINT_JS_BUILD_TOOL_EXECUTABLE_SUFFIX "")
if (CMAKE_HOST_SYSTEM_NAME STREQUAL Windows)
  set(QUICK_LINT_JS_BUILD_TOOL_EXECUTABLE_SUFFIX .exe)
endif ()

if (QUICK_LINT_JS_USE_BUILD_TOOLS)
  add_executable(quick-lint-js-generate-lex-keyword IMPORTED GLOBAL)
  set_target_properties(
    quick-lint-js-generate-lex-keyword
    PROPERTIES
    IMPORTED_LOCATION "${QUICK_LINT_JS_USE_BUILD_TOOLS}/quick-lint-js-generate-lex-keyword${QUICK_LINT_JS_BUILD_TOOL_EXECUTABLE_SUFFIX}"
  )
elseif (QUICK_LINT_JS_ENABLE_BUILD_TOOLS)
  quick_lint_js_add_executable(
    quick-lint-js-generate-lex-keyword
    generate-lex-keyword.cpp
  )
  set_target_properties(
    quick-lint-js-generate-lex-keyword
    PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ..
  )
  target_link_libraries(
    quick-lint-js-generate-lex-keyword
    PRIVATE
    quick-lint-js-tool-lib
  )
endif ()

if (QUICK_LINT_JS_USE_BUILD_TOOLS)
  add_executable(quick-lint-js-generate-lex-tables IMPORTED GLOBAL)
  set_target_properties(
    quick-lint-js-generate-lex-tables
    PROPERTIES
    IMPORTED_LOCATION "${QUICK_LINT_JS_USE_BUILD_TOOLS}/quick-lint-js-generate-lex-tables${QUICK_LINT_JS_BUILD_TOOL_EXECUTABLE_SUFFIX}"
  )
elseif (QUICK_LINT_JS_ENABLE_BUILD_TOOLS)
  quick_lint_js_add_executable(
    quick-lint-js-generate-lex-tables
    generate-lex-tables.cpp
  )
  set_target_properties(
    quick-lint-js-generate-lex-tables
    PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ..
  )
  target_link_libraries(
    quick-lint-js-generate-lex-tables
    PRIVATE
    quick-lint-js-tool-lib
  )
endif ()

if (QUICK_LINT_JS_ENABLE_BUILD_TOOLS)
  add_custom_target(
    quick-lint-js-build-tools
    COMMENT "Building all quick-lint-js build-time tools"
    DEPENDS
    quick-lint-js-generate-lex-keyword
    quick-lint-js-generate-lex-tables
  )
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
