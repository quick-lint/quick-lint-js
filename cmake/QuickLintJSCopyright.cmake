# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

include(QuickLintJSCompiler)

set(QUICK_LINT_JS_COLLECT_COPYRIGHT "${CMAKE_CURRENT_LIST_DIR}/../tools/collect-copyright")

function (quick_lint_js_collect_copyright NAME)
  cmake_parse_arguments(
    ""
    ""
    "OUTPUT_FILE;TARGET"
    "EXTRA_VENDOR_PROJECTS"
    ${ARGN}
  )
  set(LINKMAP_FILE "${CMAKE_CURRENT_BINARY_DIR}/${_TARGET}.trace")

  set(ERROR_MESSAGE_SEVERITY WARNING)
  if ($ENV{QLJS_COLLECT_COPYRIGHT_NO_WARNINGS})
    set(ERROR_MESSAGE_SEVERITY FATAL_ERROR)
  endif ()

  set(LICENSE_LINKMAP_TYPE)
  if (EMSCRIPTEN)
    set(LICENSE_LINKMAP_TYPE emscripten)
  elseif (CMAKE_SYSTEM_NAME STREQUAL Darwin)
    set(LICENSE_LINKMAP_TYPE macho)
  elseif (CMAKE_SYSTEM_NAME STREQUAL Linux)
    set(LICENSE_LINKMAP_TYPE elf)
  elseif (CMAKE_SYSTEM_NAME STREQUAL Windows AND MSVC)
    set(LICENSE_LINKMAP_TYPE pe)
  elseif (CMAKE_SYSTEM_NAME STREQUAL Windows AND MINGW)
    quick_lint_js_classify_linker()
    if ("${QUICK_LINT_JS_CXX_LINKER_TYPE}" STREQUAL "GNU ld")
      set(LICENSE_LINKMAP_TYPE elf)
    elseif ("${QUICK_LINT_JS_CXX_LINKER_TYPE}" STREQUAL "LLVM LLD PE")
      set(LICENSE_LINKMAP_TYPE coff-lld)
    endif ()
  endif ()
  if ("${LICENSE_LINKMAP_TYPE}" STREQUAL "")
    message("${ERROR_MESSAGE_SEVERITY}" "Unrecognized platform. Not generating copyright file.")
    return ()
  endif ()

  find_package(PythonInterp 3.6)
  if (NOT PYTHONINTERP_FOUND)
    message("${ERROR_MESSAGE_SEVERITY}" "Python interpreter not found. Not generating copyright file.")
    return ()
  endif ()

  if (LICENSE_LINKMAP_TYPE STREQUAL emscripten AND NOT CMAKE_GENERATOR STREQUAL Ninja)
    message("${ERROR_MESSAGE_SEVERITY}" "${CMAKE_GENERATOR} not supported. Not generating copyright file.")
    return()
  endif ()

  set(COLLECT_COPYRIGHT_OPTIONS)
  if (LICENSE_LINKMAP_TYPE STREQUAL coff-lld)
    target_link_libraries(
      "${_TARGET}"
      PRIVATE
      "-Wl,-Xlink,-reproduce:${LINKMAP_FILE}"
    )
  endif ()
  if (LICENSE_LINKMAP_TYPE STREQUAL emscripten)
    target_link_libraries(
      "${_TARGET}"
      PRIVATE
      # HACK(strager): Redirect --trace output to a file. Doesn't work with the
      # Unix Makefiles generator, but does work with the Ninja generator.
      "-Wl,--trace >${LINKMAP_FILE}"
    )
    list(APPEND COLLECT_COPYRIGHT_OPTIONS --cross-compiling-emscripten)
  endif ()
  if (LICENSE_LINKMAP_TYPE STREQUAL elf)
    target_link_libraries(
      "${_TARGET}"
      PRIVATE
      "-Wl,-Map,${LINKMAP_FILE}"
    )
  endif ()
  if (LICENSE_LINKMAP_TYPE STREQUAL macho)
    target_link_libraries(
      "${_TARGET}"
      PRIVATE
      "-Wl,-map,${LINKMAP_FILE}"
    )
  endif ()
  if (LICENSE_LINKMAP_TYPE STREQUAL pe)
    target_link_libraries(
      "${_TARGET}"
      PRIVATE
      "-MAP:${LINKMAP_FILE}"
    )
  endif ()

  if (_EXTRA_VENDOR_PROJECTS)
    list(
      APPEND
      COLLECT_COPYRIGHT_OPTIONS
      --extra-vendor-projects
      ${_EXTRA_VENDOR_PROJECTS}
    )
  endif ()

  add_custom_command(
    OUTPUT "${_OUTPUT_FILE}"
    COMMAND
      "${PYTHON_EXECUTABLE}"
      "${QUICK_LINT_JS_COLLECT_COPYRIGHT}"
      --build-directory
      "${CMAKE_BINARY_DIR}"
      --linkmap
      "${LINKMAP_FILE}"
      ${COLLECT_COPYRIGHT_OPTIONS}
      >"${_OUTPUT_FILE}"
    DEPENDS
      "${QUICK_LINT_JS_COLLECT_COPYRIGHT}"
      "${_TARGET}"
  )
  add_custom_target("${NAME}" ALL DEPENDS "${_OUTPUT_FILE}")
endfunction ()

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
