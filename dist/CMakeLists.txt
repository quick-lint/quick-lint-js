# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

cmake_minimum_required(VERSION 3.10)
include(GNUInstallDirs)

# Install AppStream files to /usr/share/metainfo/%{id}.metainfo.xml.
# https://www.freedesktop.org/software/appstream/docs/chap-Quickstart.html
install(
  FILES appstream/com.quick_lint.quick_lint_js.metainfo.xml
  DESTINATION "${CMAKE_INSTALL_DATAROOTDIR}/metainfo"
)

# Install icons referenced by the AppStream metadata.
install(
  FILES artwork/dusty-right.svg
  DESTINATION "${CMAKE_INSTALL_DATAROOTDIR}/icons/hicolor/scalable/apps"
  RENAME com.quick_lint.quick_lint_js.svg
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
