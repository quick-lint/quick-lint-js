# Copyright (C) 2020  Matthew Glazar
# See end of file for extended copyright information.

# This file is a library used by other script. Do not execute this file
# directly.

find_non_vendor_files() {
  git ls-files --cached --exclude-standard --others \
    | { grep -v '^vendor/.*/' || true ; } \
    | {
      while read path ; do
        if [ -f "${path}" ] ; then
          printf '%s\n' "${path}"
        fi
      done
   }
}

matching() {
  local pattern="${1}"
  grep -E "${pattern}" || true
}

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
