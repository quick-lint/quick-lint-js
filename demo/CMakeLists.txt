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
  quick_lint_js_add_executable(
    quick-lint-js-wasm-demo
    quick-lint-js/wasm-demo.h
    wasm-demo.cpp
  )
  target_include_directories(quick-lint-js-wasm-demo PRIVATE .)
  target_link_libraries(
    quick-lint-js-wasm-demo
    PRIVATE
    quick-lint-js-lib
    "-sEXPORTED_FUNCTIONS='[\"_quick_lint_js_parse_and_lint_for_wasm_demo\"]'"
    "-sEXPORTED_RUNTIME_METHODS='[\"ccall\", \"cwrap\"]'"
    "-sMODULARIZE"
    "-sEXPORT_NAME=QUICK_LINT_JS_WASM_DEMO"
    # TODO(strager): Use -sEXPORT_ES6 when it works with Node.js.
  )

  set(WASM_DEMO_DIST_DIR dist)
  install(
    TARGETS quick-lint-js-wasm-demo
    RUNTIME
    COMPONENT wasm-demo
    EXCLUDE_FROM_ALL
    DESTINATION "${WASM_DEMO_DIST_DIR}"
  )
  install(
    PROGRAMS $<TARGET_FILE_DIR:quick-lint-js-wasm-demo>/$<TARGET_FILE_PREFIX:quick-lint-js-wasm-demo>$<TARGET_FILE_BASE_NAME:quick-lint-js-wasm-demo>.wasm
    DESTINATION "${WASM_DEMO_DIST_DIR}"
    COMPONENT wasm-demo
  )
endif ()
