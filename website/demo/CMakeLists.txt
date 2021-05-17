# Copyright (C) 2020  Matthew Glazar
# See end of file for extended copyright information.

cmake_minimum_required(VERSION 3.10)
include(QuickLintJSCopyright)
include(QuickLintJSTarget)

if (EMSCRIPTEN)
  quick_lint_js_add_executable(
    quick-lint-js-web-demo
    quick-lint-js/web-demo.h
    web-demo.cpp
  )
  target_include_directories(quick-lint-js-web-demo PRIVATE .)
  target_link_libraries(
    quick-lint-js-web-demo
    PRIVATE
    quick-lint-js-lib
    "-sEXPORTED_FUNCTIONS='[\"_quick_lint_js_parse_and_lint_for_web_demo\"]'"
    "-sEXPORTED_RUNTIME_METHODS='[\"ccall\", \"cwrap\"]'"
    "-sMODULARIZE"
    "-sEXPORT_NAME=QUICK_LINT_JS_WEB_DEMO"
    # TODO(strager): Use -sEXPORT_ES6 when it works with Node.js.
  )

  set(web_demo_DIST_DIR dist)
  install(
    TARGETS quick-lint-js-web-demo
    RUNTIME
    COMPONENT web-demo
    EXCLUDE_FROM_ALL
    DESTINATION "${web_demo_DIST_DIR}"
  )
  install(
    PROGRAMS $<TARGET_FILE_DIR:quick-lint-js-web-demo>/$<TARGET_FILE_PREFIX:quick-lint-js-web-demo>$<TARGET_FILE_BASE_NAME:quick-lint-js-web-demo>.wasm
    DESTINATION "${web_demo_DIST_DIR}"
    COMPONENT web-demo
  )

  quick_lint_js_collect_copyright(
    quick-lint-js-web-demo-licenses
    TARGET quick-lint-js-web-demo
    OUTPUT_FILE "${CMAKE_CURRENT_BINARY_DIR}/quick-lint-js-web-demo-licenses.txt"
  )
  if (TARGET quick-lint-js-web-demo-licenses)
    install(
      FILES "${CMAKE_CURRENT_BINARY_DIR}/quick-lint-js-web-demo-licenses.txt"
      DESTINATION "${web_demo_DIST_DIR}"
      RENAME copyright.txt
      COMPONENT web-demo
    )
  endif ()
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
