# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

cmake_minimum_required(VERSION 3.10)
include(QuickLintJSAppleCodesign)
include(QuickLintJSCopyright)
include(QuickLintJSNodeJS)
include(QuickLintJSTarget)

set(VSCODE_DIST_DIR dist)

if (QUICK_LINT_JS_ENABLE_VSCODE AND NOT EMSCRIPTEN)
  if (QUICK_LINT_JS_NODEJS_OS_PLATFORM AND QUICK_LINT_JS_NODEJS_OS_ARCH)
    quick_lint_js_add_library(
      quick-lint-js-vscode-node
      SHARED
      quick-lint-js-vscode-node.cpp
      quick-lint-js/napi-support.h
      quick-lint-js/vscode-error-reporter.h
      quick-lint-js/vscode.h
    )
    set_target_properties(
      quick-lint-js-vscode-node
      PROPERTIES
      PREFIX ""
      SUFFIX "_${QUICK_LINT_JS_NODEJS_OS_PLATFORM}-${QUICK_LINT_JS_NODEJS_OS_ARCH}.node"
    )
    target_compile_definitions(quick-lint-js-vscode-node PRIVATE NAPI_VERSION=6)
    target_include_directories(quick-lint-js-vscode-node PRIVATE .)
    target_link_libraries(
      quick-lint-js-vscode-node
      PRIVATE
      node-addon-api
      quick-lint-js-lib
    )

    if (QUICK_LINT_JS_ENABLE_APPLE_CODESIGN)
      quick_lint_js_apple_codesign(quick-lint-js-vscode-node)
    endif ()

    install(
      TARGETS quick-lint-js-vscode-node
      LIBRARY  # Linux DSO; macOS dylib
        COMPONENT vscode-node
        EXCLUDE_FROM_ALL
        DESTINATION "${VSCODE_DIST_DIR}"
      RUNTIME  # Windows DLL
        COMPONENT vscode-node
        EXCLUDE_FROM_ALL
        DESTINATION "${VSCODE_DIST_DIR}"
    )

    if (CMAKE_SYSTEM_NAME STREQUAL Linux)
      target_link_libraries(
        quick-lint-js-vscode-node
        PRIVATE
        -Wl,--version-script=${CMAKE_CURRENT_SOURCE_DIR}/quick-lint-js-vscode-node.map
      )
    endif ()
    if (CMAKE_SYSTEM_NAME STREQUAL Darwin)
      target_link_libraries(
        quick-lint-js-vscode-node
        PRIVATE
        -Wl,-dead_strip
        -Wl,-exported_symbols_list,${CMAKE_CURRENT_SOURCE_DIR}/quick-lint-js-vscode-node.exports
      )
    endif ()

    quick_lint_js_collect_copyright(
      quick-lint-js-vscode-node-licenses
      TARGET quick-lint-js-vscode-node
      OUTPUT_FILE quick-lint-js-vscode-node-licenses.txt
      EXTRA_VENDOR_PROJECTS node node-addon-api
    )
    if (TARGET quick-lint-js-vscode-node-licenses)
      install(
        FILES "${CMAKE_CURRENT_BINARY_DIR}/quick-lint-js-vscode-node-licenses.txt"
        COMPONENT vscode-node
        DESTINATION "${VSCODE_DIST_DIR}"
        EXCLUDE_FROM_ALL
        RENAME "copyright_quick-lint-js-vscode-node_${QUICK_LINT_JS_NODEJS_OS_PLATFORM}-${QUICK_LINT_JS_NODEJS_OS_ARCH}.txt"
      )
    endif ()
  else ()
    if (NOT QUICK_LINT_JS_NODEJS_OS_PLATFORM)
      message(WARNING "Could not determine Node.js os.platform (QUICK_LINT_JS_NODEJS_OS_PLATFORM) (CMAKE_SYSTEM_NAME is ${CMAKE_SYSTEM_NAME}); not building Node.js extension")
    else ()
      message(WARNING "Could not determine Node.js os.arch (QUICK_LINT_JS_NODEJS_OS_ARCH) (CMAKE_SYSTEM_PROCESSOR is ${CMAKE_SYSTEM_PROCESSOR}); not building Node.js extension")
    endif ()
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
