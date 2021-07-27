# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

cmake_minimum_required(VERSION 3.10)
include(QuickLintJSCopyright)
include(QuickLintJSTarget)

set(VSCODE_DIST_DIR dist)

if (EMSCRIPTEN)
  # TODO(strager): Rename.
  quick_lint_js_add_executable(quick-lint-js-vscode empty.cpp)
  target_link_libraries(
    quick-lint-js-vscode
    PRIVATE
    quick-lint-js-lib
    "-sEXPORTED_FUNCTIONS=[\"_qljs_vscode_create_workspace\",\"_qljs_vscode_destroy_workspace\",\"_qljs_vscode_create_source_document\",\"_qljs_vscode_destroy_document\",\"_qljs_vscode_replace_text\",\"_qljs_vscode_lint\",\"_qljs_web_demo_create_document\",\"_qljs_web_demo_destroy_document\",\"_qljs_web_demo_set_text\",\"_qljs_web_demo_lint\",\"_malloc\",\"_free\"]"
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

if (QUICK_LINT_JS_ENABLE_VSCODE AND NOT EMSCRIPTEN)
  if (NOT QUICK_LINT_JS_NODEJS_OS_PLATFORM)
    if (CMAKE_SYSTEM_NAME STREQUAL Darwin)
      set(NODEJS_OS_PLATFORM darwin)
    elseif (CMAKE_SYSTEM_NAME STREQUAL Linux)
      set(NODEJS_OS_PLATFORM linux)
    elseif (CMAKE_SYSTEM_NAME STREQUAL Windows)
      set(NODEJS_OS_PLATFORM win32)
    else ()
      set(NODEJS_OS_PLATFORM)
    endif ()
    set(
      QUICK_LINT_JS_NODEJS_OS_PLATFORM
      "${NODEJS_OS_PLATFORM}"
      CACHE STRING
      "Node.js value of os.platform() for the target system"
      FORCE
    )
  endif ()
  if (NOT QUICK_LINT_JS_NODEJS_OS_ARCH)
    if (CMAKE_SYSTEM_PROCESSOR STREQUAL AMD64 OR CMAKE_SYSTEM_PROCESSOR STREQUAL x86_64)
      set(NODEJS_OS_ARCH x64)
    else ()
      set(NODEJS_OS_ARCH)
    endif ()
    set(
      QUICK_LINT_JS_NODEJS_OS_ARCH
      "${NODEJS_OS_ARCH}"
      CACHE STRING
      "Node.js value of os.arch() for the target system"
      FORCE
    )
  endif ()

  if (QUICK_LINT_JS_NODEJS_OS_PLATFORM AND QUICK_LINT_JS_NODEJS_OS_ARCH)
    quick_lint_js_add_library(quick-lint-js-node-fs SHARED node-fs.cpp)
    set_target_properties(
      quick-lint-js-node-fs
      PROPERTIES
      PREFIX ""
      SUFFIX "_${QUICK_LINT_JS_NODEJS_OS_PLATFORM}-${QUICK_LINT_JS_NODEJS_OS_ARCH}.node"
    )
    target_compile_definitions(quick-lint-js-node-fs PRIVATE NAPI_VERSION=6)
    target_link_libraries(
      quick-lint-js-node-fs
      PRIVATE
      node-addon-api
      quick-lint-js-lib
    )
    install(
      TARGETS quick-lint-js-node-fs
      LIBRARY  # Linux DSO; macOS dylib
        COMPONENT vscode
        EXCLUDE_FROM_ALL
        DESTINATION "${VSCODE_DIST_DIR}"
      RUNTIME  # Windows DLL
        COMPONENT vscode
        EXCLUDE_FROM_ALL
        DESTINATION "${VSCODE_DIST_DIR}"
    )

    quick_lint_js_collect_copyright(
      quick-lint-js-node-fs-licenses
      TARGET quick-lint-js-node-fs
      OUTPUT_FILE quick-lint-js-node-fs-licenses.txt
      EXTRA_VENDOR_PROJECTS node node-addon-api
    )
    if (TARGET quick-lint-js-node-fs-licenses)
      install(
        FILES "${CMAKE_CURRENT_BINARY_DIR}/quick-lint-js-node-fs-licenses.txt"
        COMPONENT vscode
        DESTINATION "${VSCODE_DIST_DIR}"
        EXCLUDE_FROM_ALL
        RENAME "copyright_quick-lint-js-node-fs_${QUICK_LINT_JS_NODEJS_OS_PLATFORM}-${QUICK_LINT_JS_NODEJS_OS_ARCH}.txt"
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
