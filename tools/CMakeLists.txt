# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

cmake_minimum_required(VERSION 3.13)
include(GNUInstallDirs)
include(QuickLintJSTarget)

add_subdirectory(test-typescript)

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

set(QUICK_LINT_JS_BUILD_TOOL_TARGETS)
function (quick_lint_js_add_build_tool_executable NAME)
  set(SOURCES "${ARGN}")
  if (QUICK_LINT_JS_USE_BUILD_TOOLS)
    add_executable("${NAME}" IMPORTED GLOBAL)
    set_target_properties(
      "${NAME}"
      PROPERTIES
      IMPORTED_LOCATION "${QUICK_LINT_JS_USE_BUILD_TOOLS}/${NAME}${QUICK_LINT_JS_BUILD_TOOL_EXECUTABLE_SUFFIX}"
    )
  elseif (QUICK_LINT_JS_ENABLE_BUILD_TOOLS)
    quick_lint_js_add_executable("${NAME}" ${SOURCES})
    set_target_properties("${NAME}" PROPERTIES RUNTIME_OUTPUT_DIRECTORY ..)
    target_link_libraries("${NAME}" PRIVATE quick-lint-js-tool-lib)
    target_include_directories("${NAME}" PRIVATE .)
  endif ()

  list(APPEND QUICK_LINT_JS_BUILD_TOOL_TARGETS "${NAME}")
  set(QUICK_LINT_JS_BUILD_TOOL_TARGETS "${QUICK_LINT_JS_BUILD_TOOL_TARGETS}" PARENT_SCOPE)
endfunction ()

quick_lint_js_add_build_tool_executable(
  quick-lint-js-compile-translations
  compile-translations.cpp
)

quick_lint_js_add_build_tool_executable(
  quick-lint-js-generate-diagnostic-metadata
  generate-diagnostic-metadata.cpp
)

quick_lint_js_add_build_tool_executable(
  quick-lint-js-generate-lex-keyword
  generate-lex-keyword.cpp
)

quick_lint_js_add_build_tool_executable(
  quick-lint-js-generate-trace-sources
  generate-trace-sources.cpp
)

if (QUICK_LINT_JS_ENABLE_BUILD_TOOLS)
  add_custom_target(
    quick-lint-js-build-tools
    COMMENT "Building all quick-lint-js build-time tools"
    DEPENDS ${QUICK_LINT_JS_BUILD_TOOL_TARGETS}
  )
  install(
    TARGETS ${QUICK_LINT_JS_BUILD_TOOL_TARGETS}
    COMPONENT build-tools
    EXCLUDE_FROM_ALL
    RUNTIME
    DESTINATION "${CMAKE_INSTALL_BINDIR}"
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
