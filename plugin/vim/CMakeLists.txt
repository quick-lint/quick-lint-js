# Copyright (C) 2020  Matthew Glazar
# See end of file for extended copyright information.

cmake_minimum_required(VERSION 3.10)
include(GNUInstallDirs)

install(
  FILES quick-lint-js.vim/autoload/quick_lint_js_ale.vim
  DESTINATION "${CMAKE_INSTALL_DATAROOTDIR}/vim/addons/autoload"
)
install(
  FILES quick-lint-js.vim/ale_linters/javascript/quick_lint_js.vim
  DESTINATION "${CMAKE_INSTALL_DATAROOTDIR}/vim/addons/ale_linters/javascript"
)
install(
  FILES quick-lint-js.yaml
  DESTINATION "${CMAKE_INSTALL_DATAROOTDIR}/vim/registry"
)

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
