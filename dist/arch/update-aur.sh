#!/bin/sh

# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

set -e
set -u

here="$(cd "$(dirname "${0}")" && pwd)"

print_usage() {
  printf 'usage: %s [--docker] [--test] /path/to/quick-lint-js-aur\n' "${0}"
}

docker=0
test=0
qljsaur=

docker_image=ghcr.io/quick-lint/quick-lint-js-dist-arch:v5

while [ "${#}" -ne 0 ]; do
  case "${1}" in
    --docker) docker=1 ;;
    --test) test=1 ;;
    -*)
      printf 'error: invalid option: %s\n' "${1}"
      print_usage >&2
      exit 1
      ;;
    *) qljsaur="$(cd "${1}" && pwd)" ;;
  esac
  shift
done

if [ -z "${qljsaur}" ]; then
  print_usage >&2
  exit 1
fi

cd "${qljsaur}"
cp "${here}/PKGBUILD-release" PKGBUILD

script="
  updpkgsums PKGBUILD
  makepkg --printsrcinfo PKGBUILD >.SRCINFO
"
if [ "${test}" -eq 1 ]; then
  script="${script}
  makepkg --install --force --syncdeps --noconfirm
"
fi

if [ "${docker}" -eq 1 ]; then
  docker run --interactive --tty --mount type=bind,source="${qljsaur}",destination=/qljs-aur "${docker_image}" sh -c "
    set -e
    set -u
    cd /qljs-aur
    sudo pacman --sync --refresh
    ${script}
  "
else
  if ! command -v updpkgsums >/dev/null; then
    printf "warning: updpkgsums doesn't seem to be installed. Consider using --docker\n" >&2
  fi
  if ! command -v makepkg >/dev/null; then
    printf "warning: makepkg doesn't seem to be installed. Consider using --docker\n" >&2
  fi
  eval "${script}"
fi

git add PKGBUILD .SRCINFO
printf '\nChanges staged. Please commit and push.\n' >&2

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
