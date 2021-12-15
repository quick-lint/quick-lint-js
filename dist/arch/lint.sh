#!/usr/bin/env bash

# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

set -e
set -u

cd "$(dirname "${0}")"

errors="$(mktemp)"
trap 'rm -f "${errors}"' EXIT

# HACK(strager): Disable the unusedsodepends check. With -Wl,--gc-sections, the
# check fails on libm. Even with -Wl,--as-needed, the linker keeps the NEEDED
# entry, so I don't know how to work around the libm dependency.
namcap --exclude=unusedsodepends PKGBUILD-dev PKGBUILD-git PKGBUILD-release ./quick-lint-js-*.pkg.tar.zst |& tee "${errors}"
if [ -s "${errors}" ]; then
  printf 'error: namcap reported an error\n' >&2
  exit 1
fi

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
