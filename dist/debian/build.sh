#!/usr/bin/env bash

# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

set -e
set -u

cd "$(dirname "${0}")/../.."

package_options=()
while [ "${#}" -gt 0 ]; do
  case "${1}" in
    --xenial) package_options+=(--xenial) ;;
    *)
      printf 'error: unrecognized option: %s\n' >&2
      exit 2
      ;;
  esac
  shift
done

package_version="$(head -n1 version)"
debian_package_version="${package_version}-1"

DEB_BUILD_OPTIONS="parallel=$(nproc)"
export DEB_BUILD_OPTIONS

./dist/debian/package.sh "${package_options[@]:+${package_options[@]}}" --output-directory dist/debian/build/

(
  cd dist/debian/build/
  rm -rf "quick-lint-js-${package_version}/"
  dpkg-source --extract "quick-lint-js_${debian_package_version}.dsc"
  cd "quick-lint-js-${package_version}/"
  dpkg-buildpackage -rfakeroot -b -uc -us
)

# elf-error is noisy, so turn it of for the dbgsym package if supported:
# https://salsa.debian.org/lintian/lintian/-/merge_requests/387
dbgsym_lintian_options=()
lintian_version="$(lintian --version | grep -o '[0-9].*')"
if dpkg --compare-versions "${lintian_version}" ge 2.114.0; then
  dbgsym_lintian_options+=(--suppress-tags elf-error)
fi

./dist/debian/strict-lintian.sh "dist/debian/build/quick-lint-js_${debian_package_version}_amd64.deb"
./dist/debian/strict-lintian.sh "${dbgsym_lintian_options[@]:+${dbgsym_lintian_options[@]}}" "dist/debian/build/quick-lint-js-dbgsym_${debian_package_version}_amd64.deb"
./dist/debian/strict-lintian.sh "dist/debian/build/quick-lint-js-vim_${debian_package_version}_all.deb"

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
