# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}")
include(QuickLintJSSublimeTextPackage)

quick_lint_js_sublime_text_get_package_pathname(SUBLIME_TEXT_PACKAGE_PATHNAME)
quick_lint_js_sublime_text_get_package_files(SUBLIME_TEXT_PACKAGE_FILES)
quick_lint_js_sublime_text_get_package_destination(SUBLIME_TEXT_PACKAGE_DESTINATION)

# Create package.
execute_process(
  COMMAND
  "${CMAKE_COMMAND}" -E make_directory "${SUBLIME_TEXT_PACKAGE_PATHNAME}"
)
execute_process(
  COMMAND
  "${CMAKE_COMMAND}" -E copy ${SUBLIME_TEXT_PACKAGE_FILES} "${SUBLIME_TEXT_PACKAGE_PATHNAME}"
)

# Install package.
execute_process(
  COMMAND
  "${CMAKE_COMMAND}" -E make_directory "${SUBLIME_TEXT_PACKAGE_DESTINATION}"
)
execute_process(
  COMMAND
  "${CMAKE_COMMAND}" -E copy_directory "${SUBLIME_TEXT_PACKAGE_PATHNAME}" "${SUBLIME_TEXT_PACKAGE_DESTINATION}"
)

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
