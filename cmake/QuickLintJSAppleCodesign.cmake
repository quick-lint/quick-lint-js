# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

function (quick_lint_js_apple_codesign TARGET)
  if (CMAKE_GENERATOR STREQUAL Xcode)
    set_target_properties(
      "${TARGET}"
      PROPERTIES
      XCODE_ATTRIBUTE_CODE_SIGN_IDENTITY
      "${QUICK_LINT_JS_APPLE_CODE_SIGN_IDENTITY}"
    )
  else ()
    find_program(
      QUICK_LINT_JS_APPLE_CODESIGN
      NAMES codesign
      DOC "Program to sign macOS programs"
    )
    if (NOT QUICK_LINT_JS_APPLE_CODESIGN)
      message(FATAL_ERROR "Cannot find codesign utility for QUICK_LINT_JS_ENABLE_APPLE_CODE_SIGN")
    endif ()

    add_custom_command(
      TARGET "${TARGET}"
      POST_BUILD
      COMMAND
        "${QUICK_LINT_JS_APPLE_CODESIGN}"
        --sign "${QUICK_LINT_JS_APPLE_CODE_SIGN_IDENTITY}"
        "$<TARGET_FILE:${TARGET}>"
      COMMENT "Signing ${TARGET}"
      VERBATIM
    )
  endif ()
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
