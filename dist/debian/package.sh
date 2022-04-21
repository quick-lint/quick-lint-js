#!/usr/bin/env bash

# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

set -e
set -u

cd "$(dirname "${0}")/../.."

variant=default
output_directory=
while [ "${#}" -gt 0 ]; do
  case "${1}" in
    --xenial) variant=xenial ;;
    --output-directory) output_directory="${2}" ; shift ;;
    *)
      printf 'error: unrecognized option: %s\n' >&2
      exit 2
      ;;
  esac
  shift
done
if [ "${output_directory}" = "" ]; then
  printf 'error: missing --output-directory\n' >&2
  exit 2
fi

package_version="$(head -n1 version)"

temp_dir="$(mktemp -d)"

git archive --format tar.gz --prefix "quick-lint-js-${package_version}/" --output "${temp_dir}/quick-lint-js_${package_version}.orig.tar.gz" HEAD

tar xzf "${temp_dir}/quick-lint-js_${package_version}.orig.tar.gz" -C "${temp_dir}"
source_dir="${temp_dir}/quick-lint-js-${package_version}"

cp -aL dist/debian/debian "${source_dir}/debian"
temp_debian_dir="${source_dir}/debian"
if [ "${variant}" != default ]; then
  cp -a "dist/debian/debian/compat-${variant}" "${source_dir}/debian/compat"
  cp -a "dist/debian/debian/control-${variant}" "${source_dir}/debian/control"
  cp -a "dist/debian/debian/copyright-${variant}" "${source_dir}/debian/copyright"
  cp -a "dist/debian/debian/rules-${variant}" "${source_dir}/debian/rules"
  cp -a "dist/debian/debian/source/lintian-overrides-${variant}" "${source_dir}/debian/source/lintian-overrides"
fi
rm "${source_dir}/debian/"{compat,control,copyright,rules,source/lintian-overrides}-*

(
  cd "${source_dir}"
  dpkg-buildpackage -S -d -uc -us
)

mkdir -p "${output_directory}"
cp "${temp_dir}/quick-lint-js_${package_version}"-*.debian.tar.xz "${output_directory}"
cp "${temp_dir}/quick-lint-js_${package_version}"-*.dsc "${output_directory}"
cp "${temp_dir}/quick-lint-js_${package_version}"-*_source.changes "${output_directory}"
cp "${temp_dir}/quick-lint-js_${package_version}.orig.tar.gz" "${output_directory}"

# HACK(strager): Some versions of dpkg-buildpackage run dpkg-buildinfo, and some
# don't. Copy the buildinfo file if it exists. Otherwise, lintian complains
# about the missing buildinfo file.
if [ -f "${temp_dir}/quick-lint-js_${package_version}"-*_source.buildinfo ]; then
  cp "${temp_dir}/quick-lint-js_${package_version}"-*_source.buildinfo "${output_directory}"
fi

rm -r "${temp_dir}"

suppressed_tags=newer-standards-version
lintian_version="$(lintian --version | grep -o '[0-9].*')"
if dpkg --compare-versions "${lintian_version}" ge 2.62.0; then
  # We built the orig tarball from Git, so it is not signed. Don't complain
  # about a missing signature.
  suppressed_tags="${suppressed_tags},orig-tarball-missing-upstream-signature"
fi
if dpkg --compare-versions 2.114.0 ge "${lintian_version}"; then
  # Our suppression for source-is-missing only works for more recent versions
  # of lintian.
  suppressed_tags="${suppressed_tags},source-is-missing"
fi

# Run linting after copying to the output directory in case the user wants to
# build the package anyway.
./dist/debian/strict-lintian.sh \
  --suppress-tags "${suppressed_tags}" \
  --profile debian \
  "${output_directory}/quick-lint-js_${package_version}"-*_source.changes

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
