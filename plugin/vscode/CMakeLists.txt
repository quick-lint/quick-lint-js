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
include(QuickLintJSCopyright)
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

  quick_lint_js_collect_copyright(
    quick-lint-js-vscode-licenses
    TARGET quick-lint-js-vscode
    OUTPUT_FILE quick-lint-js-vscode-licenses.txt
  )
  if (TARGET quick-lint-js-vscode-licenses)
    install(
      FILES "${CMAKE_CURRENT_BINARY_DIR}/quick-lint-js-vscode-licenses.txt"
      COMPONENT vscode
      DESTINATION "${VSCODE_DIST_DIR}"
      EXCLUDE_FROM_ALL
      RENAME copyright.txt
    )
  endif ()
endif ()
