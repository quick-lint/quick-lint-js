# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

function (quick_lint_js_apple_codesign TARGET)
  if (CMAKE_CONFIGURATION_TYPES)
    foreach (CONFIG IN LISTS CMAKE_CONFIGURATION_TYPES)
      quick_lint_js_apple_codesign_config("${TARGET}" "${CONFIG}")
    endforeach ()
  else ()
    quick_lint_js_apple_codesign_config("${TARGET}" "${CMAKE_BUILD_TYPE}")
  endif ()
endfunction ()

# quick_lint_js_apple_codesign must have been called on the given targets first.
#
# Required:
# * DESTINATION
# * TARGETS
#
# Optional:
# * COMPONENT
function (quick_lint_js_apple_codesign_install)
  cmake_parse_arguments("" "" "COMPONENT;DESTINATION" "TARGETS" ${ARGN})

  set(INSTALL_OPTIONS)
  if (_COMPONENT)
    list(APPEND INSTALL_OPTIONS COMPONENT "${_COMPONENT}")
  endif ()
  list(APPEND INSTALL_OPTIONS DESTINATION "${_DESTINATION}")

  foreach (TARGET IN LISTS _TARGETS)
    if (CMAKE_CONFIGURATION_TYPES)
      foreach (CONFIG IN LISTS CMAKE_CONFIGURATION_TYPES)
        quick_lint_js_apple_codesign_install_config("${TARGET}" "${CONFIG}" "${INSTALL_OPTIONS}")
      endforeach ()
    else ()
      quick_lint_js_apple_codesign_install_config("${TARGET}" "${CMAKE_BUILD_TYPE}" "${INSTALL_OPTIONS}")
    endif ()
  endforeach ()
endfunction ()

function (quick_lint_js_apple_codesign_install_config TARGET CONFIG INSTALL_OPTIONS)
  quick_lint_js_apple_codesign_get_signed_path("${TARGET}" "${CONFIG}" SIGNED_PATH)
  quick_lint_js_get_target_runtime_output_path_components("${TARGET}" "${CONFIG}" ORIGINAL)
  install(
    PROGRAMS "${SIGNED_PATH}"
    CONFIGURATIONS "${CONFIG}"
    RENAME "${ORIGINAL_PREFIX}${ORIGINAL_RUNTIME_OUTPUT_NAME}${ORIGINAL_SUFFIX}"
    ${INSTALL_OPTIONS}
  )
endfunction ()

function (quick_lint_js_get_target_runtime_output_path_components TARGET CONFIG OUTPUT_VARIABLE_PREFIX)
  quick_lint_js_get_first_target_property(
    "${TARGET}" TARGET_RUNTIME_OUTPUT_DIRECTORY
    "RUNTIME_OUTPUT_DIRECTORY_${CONFIG}"
    RUNTIME_OUTPUT_DIRECTORY
  )
  quick_lint_js_get_first_target_property("${TARGET}" TARGET_PREFIX PREFIX)
  quick_lint_js_get_first_target_property(
    "${TARGET}" TARGET_RUNTIME_OUTPUT_NAME
    "RUNTIME_OUTPUT_NAME_${CONFIG}"
    RUNTIME_OUTPUT_NAME
    "OUTPUT_NAME_${CONFIG}"
    OUTPUT_NAME
    NAME
  )
  quick_lint_js_get_first_target_property("${TARGET}" TARGET_SUFFIX SUFFIX)

  set("${OUTPUT_VARIABLE_PREFIX}_RUNTIME_OUTPUT_DIRECTORY" "${TARGET_RUNTIME_OUTPUT_DIRECTORY}" PARENT_SCOPE)
  set("${OUTPUT_VARIABLE_PREFIX}_PREFIX" "${TARGET_PREFIX}" PARENT_SCOPE)
  set("${OUTPUT_VARIABLE_PREFIX}_RUNTIME_OUTPUT_NAME" "${TARGET_RUNTIME_OUTPUT_NAME}" PARENT_SCOPE)
  set("${OUTPUT_VARIABLE_PREFIX}_SUFFIX" "${TARGET_SUFFIX}" PARENT_SCOPE)
endfunction ()

function (quick_lint_js_apple_codesign_get_signed_path TARGET CONFIG OUTPUT_VARIABLE)
  quick_lint_js_get_target_runtime_output_path_components("${TARGET}" "${CONFIG}" "")
  set(
    SIGNED_PATH
    "${_RUNTIME_OUTPUT_DIRECTORY}/${_PREFIX}${_RUNTIME_OUTPUT_NAME}.signed${_SUFFIX}"
  )
  if (NOT IS_ABSOLUTE SIGNED_PATH)
    set(SIGNED_PATH "${CMAKE_CURRENT_BINARY_DIR}/${SIGNED_PATH}")
  endif ()
  set(
    "${OUTPUT_VARIABLE}"
    "${SIGNED_PATH}"
    PARENT_SCOPE
  )
endfunction ()

function (quick_lint_js_apple_codesign_config TARGET CONFIG)
  find_program(
    QUICK_LINT_JS_APPLE_CODESIGN
    NAMES codesign
    DOC "Program to sign macOS programs"
  )
  if (NOT QUICK_LINT_JS_APPLE_CODESIGN)
    message(FATAL_ERROR "Cannot find codesign utility for QUICK_LINT_JS_ENABLE_APPLE_CODESIGN")
  endif ()

  quick_lint_js_apple_codesign_get_signed_path("${TARGET}" "${CONFIG}" SIGNED_PATH)
  add_custom_command(
    OUTPUT "${SIGNED_PATH}"
    COMMAND "${CMAKE_COMMAND}" -E copy "$<TARGET_FILE:${TARGET}>" "${SIGNED_PATH}"
    COMMAND
      "${QUICK_LINT_JS_APPLE_CODESIGN}"
      -s "${QUICK_LINT_JS_APPLE_CODESIGN_IDENTITY}"
      "${SIGNED_PATH}"
    VERBATIM
    DEPENDS "${TARGET}"
    COMMENT "Signing ${TARGET}"
  )
  add_custom_target("${TARGET}-signed" ALL DEPENDS "${SIGNED_PATH}")
endfunction ()

function (quick_lint_js_get_first_target_property TARGET OUTPUT_VARIABLE)
  foreach (PROPERTY IN LISTS ARGN)
    get_target_property(OUT "${TARGET}" "${PROPERTY}")
    if (OUT)
      set("${OUTPUT_VARIABLE}" "${OUT}" PARENT_SCOPE)
      return()
    endif ()
  endforeach ()
  set("${OUTPUT_VARIABLE}" "" PARENT_SCOPE)
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
