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

cmake_minimum_required(VERSION 3.10)
include(QuickLintJSTarget)

if (EMSCRIPTEN)
  quick_lint_js_add_executable(quick-lint-js-vscode empty.cpp)
  target_link_libraries(
    quick-lint-js-vscode
    PRIVATE
    quick-lint-js-lib
    "-sEXPORTED_FUNCTIONS='[\"_qljs_vscode_create_parser\",\"_qljs_vscode_destroy_parser\",\"_qljs_vscode_replace_text\",\"_qljs_vscode_lint\",\"_malloc\",\"_free\"]'"
    -sASSERTIONS=0
    -sSTANDALONE_WASM=1
    --no-entry
  )
  set_target_properties(quick-lint-js-vscode PROPERTIES SUFFIX .wasm)

  set(VSCODE_DIST_DIR dist)
  install(
    TARGETS quick-lint-js-vscode
    RUNTIME
    COMPONENT vscode
    EXCLUDE_FROM_ALL
    DESTINATION "${VSCODE_DIST_DIR}"
  )

  set(ERROR_MESSAGE_SEVERITY WARNING)
  if ($ENV{QLJS_COLLECT_COPYRIGHT_NO_WARNINGS})
    set(ERROR_MESSAGE_SEVERITY FATAL_ERROR)
  endif ()
  find_package(PythonInterp 3.6)
  if (NOT PYTHONINTERP_FOUND)
    message("${ERROR_MESSAGE_SEVERITY}" "Python interpreter not found. Not generating copyright file.")
  elseif (NOT CMAKE_GENERATOR STREQUAL Ninja)
    message("${ERROR_MESSAGE_SEVERITY}" "${CMAKE_GENERATOR} not supported. Not generating copyright file.")
  else ()
    target_link_libraries(
      quick-lint-js-vscode
      PRIVATE
      # HACK(strager): Redirect --trace output to a file. Doesn't work with the
      # Unix Makefiles generator, but does work with the Ninja generator.
      "-Wl,--trace >${CMAKE_CURRENT_BINARY_DIR}/quick-lint-js-vscode.trace"
    )

    add_custom_command(
      OUTPUT quick-lint-js-vscode-licenses.txt
      COMMAND
        "${PYTHON_EXECUTABLE}"
        "${CMAKE_CURRENT_SOURCE_DIR}/../../tools/collect-copyright"
        --cross-compiling-emscripten
        --linkmap
        "${CMAKE_CURRENT_BINARY_DIR}/quick-lint-js-vscode.trace"
        >quick-lint-js-vscode-licenses.txt
      DEPENDS
        "${CMAKE_CURRENT_SOURCE_DIR}/../../tools/collect-copyright"
        quick-lint-js-vscode
    )
    add_custom_target(
      quick-lint-js-vscode-licenses
      ALL
      DEPENDS quick-lint-js-vscode-licenses.txt
    )
    install(
      FILES "${CMAKE_CURRENT_BINARY_DIR}/quick-lint-js-vscode-licenses.txt"
      COMPONENT vscode
      DESTINATION "${VSCODE_DIST_DIR}"
      EXCLUDE_FROM_ALL
      RENAME copyright.txt
    )
  endif ()
endif ()
