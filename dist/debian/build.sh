#!/bin/sh

# Copyright (C) 2020  Matthew Glazar
# See end of file for extended copyright information.

set -e
set -u
set -x

cd "$(dirname "${0}")/../.."

DEB_BUILD_OPTIONS="parallel=$(nproc)"
export DEB_BUILD_OPTIONS

git archive --format tar.gz --prefix quick-lint-js-0.1.0/ --output dist/debian/quick-lint-js_0.1.0.orig.tar.gz HEAD

cd dist/debian/
rm -rf quick-lint-js-0.1.0/
tar xzf quick-lint-js_0.1.0.orig.tar.gz
cp -a debian quick-lint-js-0.1.0/debian

cd quick-lint-js-0.1.0/
dpkg-buildpackage -rfakeroot -uc -us

cd ../
lintian quick-lint-js_0.1.0-1_amd64.deb
lintian quick-lint-js-dbgsym_0.1.0-1_amd64.deb
lintian quick-lint-js-vim_0.1.0-1_all.deb

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
