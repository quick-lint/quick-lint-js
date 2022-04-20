#!/bin/sh

# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

set -e
set -u
set -x

cd "$(dirname "${0}")/../.."

variant=default
while [ "${#}" -gt 0 ]; do
  case "${1}" in
    --xenial) variant=xenial ;;
    *)
      printf 'error: unrecognized option: %s\n' >&2
      exit 2
      ;;
  esac
  shift
done

package_version="$(head -n1 version)"

DEB_BUILD_OPTIONS="parallel=$(nproc)"
export DEB_BUILD_OPTIONS

git archive --format tar.gz --prefix "quick-lint-js-${package_version}/" --output "dist/debian/quick-lint-js_${package_version}.orig.tar.gz" HEAD

cd dist/debian/
rm -rf "quick-lint-js-${package_version}/"
tar xzf "quick-lint-js_${package_version}.orig.tar.gz"
cp -a debian "quick-lint-js-${package_version}/debian"

cd "quick-lint-js-${package_version}/"
if [ "${variant}" != default ]; then
  cp -a "debian/rules-${variant}" debian/rules
fi
rm debian/rules-*

dpkg-buildpackage -rfakeroot -uc -us

errors="$(mktemp)"
trap 'rm -f "${errors}"' EXIT
strict_lintian() {
  lintian "${@}" | tee "${errors}"
  if [ -s "${errors}" ]; then
    printf 'error: lintian reported an error\n' >&2
    exit 1
  fi
}

cd ../
strict_lintian "quick-lint-js_${package_version}-1_amd64.deb"
strict_lintian "quick-lint-js-dbgsym_${package_version}-1_amd64.deb"
strict_lintian "quick-lint-js-vim_${package_version}-1_all.deb"

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
