# Copyright (C) 2020  Matthew Glazar
# See end of file for extended copyright information.

set(QUICK_LINT_JS_COLLECT_COPYRIGHT "${CMAKE_CURRENT_LIST_DIR}/../tools/collect-copyright")

function (quick_lint_js_collect_copyright NAME)
  cmake_parse_arguments("" "" "OUTPUT_FILE;TARGET" "" ${ARGN})
  set(LINKMAP_FILE "${CMAKE_CURRENT_BINARY_DIR}/${_TARGET}.trace")

  set(ERROR_MESSAGE_SEVERITY WARNING)
  if ($ENV{QLJS_COLLECT_COPYRIGHT_NO_WARNINGS})
    set(ERROR_MESSAGE_SEVERITY FATAL_ERROR)
  endif ()

  if (EMSCRIPTEN)
    set(LICENSE_LINKMAP_TYPE emscripten)
  elseif (CMAKE_SYSTEM_NAME STREQUAL Darwin)
    set(LICENSE_LINKMAP_TYPE macho)
  elseif (CMAKE_SYSTEM_NAME STREQUAL Linux)
    set(LICENSE_LINKMAP_TYPE elf)
  elseif (CMAKE_SYSTEM_NAME STREQUAL Windows AND MSVC)
    set(LICENSE_LINKMAP_TYPE pe)
  else ()
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

  add_custom_command(
    OUTPUT "${_OUTPUT_FILE}"
    COMMAND
      "${PYTHON_EXECUTABLE}"
      "${QUICK_LINT_JS_COLLECT_COPYRIGHT}"
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
