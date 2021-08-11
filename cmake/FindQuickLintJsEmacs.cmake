# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

if (QUICK_LINT_JS_EMACS)
  set(QUICK_LINT_JS_EMACS "${QUICK_LINT_JS_EMACS}")
else ()
  find_program(QUICK_LINT_JS_EMACS "emacs")
endif ()

if (NOT QUICK_LINT_JS_EMACS)
  message(WARNING "Emacs not found. Skipping... ")
  return ()
endif ()

execute_process(
  COMMAND
    ${QUICK_LINT_JS_EMACS}
    -Q -batch
    --eval "(princ (format \"%s.%s\" emacs-major-version emacs-minor-version))"
  RESULT_VARIABLE EMACS_EXIT_CODE
  OUTPUT_VARIABLE EMACS_VERSION)

if (NOT EMACS_EXIT_CODE EQUAL 0)
  message(WARNING "Emacs (${QUICK_LINT_JS_EMACS}) found but can't get its version. Skipping...")
  return ()
endif()

if (NOT EMACS_VERSION GREATER_EQUAL 24.5)
  message(WARNING "Emacs found (${QUICK_LINT_JS_EMACS}), but version ${EMACS_VERSION} is not supported. Skipping...")
  return ()
endif ()

set(QUICK_LINT_JS_EMACS_FOUND TRUE)
message(STATUS "Found Emacs: (${QUICK_LINT_JS_EMACS}) suitable version ${EMACS_VERSION} minimum required is 24.5")

macro(emacs_pkg_target NAME FILE)
  cmake_parse_arguments(
    ""
    ""
    "OUTPUT"
    ""
    ${ARGN})

  add_custom_command(
    OUTPUT ${_OUTPUT}
    COMMAND
      ${QUICK_LINT_JS_EMACS}
      -Q -batch
      -l package --eval "(package-initialize)"
      --eval "(add-to-list 'package-directory-list \"${CMAKE_CACHEFILE_DIR}\")"
      -l ${CMAKE_CURRENT_LIST_DIR}/quicklintjs-pkg.el
      -f quicklintjs-batch-make-pkg
      ${FILE}
    DEPENDS
      ${QUICK_LINT_JS_EMACS}
      ${CMAKE_CURRENT_LIST_DIR}/quicklintjs-pkg.el
      ${FILE}
    VERBATIM)

  add_custom_target(${NAME} ALL DEPENDS ${_OUTPUT})
endmacro ()

mark_as_advanced(QUICK_LINT_JS_EMACS)

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
