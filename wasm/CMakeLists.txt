# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

cmake_minimum_required(VERSION 3.10)
include(QuickLintJSCopyright)
include(QuickLintJSNodeJS)
include(QuickLintJSTarget)

set(VSCODE_DIST_DIR dist)

if (EMSCRIPTEN)
  # TODO(strager): Rename.
  quick_lint_js_add_executable(quick-lint-js-vscode empty.cpp)
  target_link_libraries(
    quick-lint-js-vscode
    PRIVATE
    quick-lint-js-lib
    "-sEXPORTED_FUNCTIONS=[\"_qljs_web_demo_create_document\",\"_qljs_web_demo_destroy_document\",\"_qljs_web_demo_set_text\",\"_qljs_web_demo_lint\",\"_malloc\",\"_free\"]"
    -sASSERTIONS=0
    -sSTANDALONE_WASM=1
    --no-entry
  )
  set_target_properties(quick-lint-js-vscode PROPERTIES SUFFIX .wasm)

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
