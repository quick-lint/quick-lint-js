#!/usr/bin/env bash

# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

set -e
set -u

ssh_args=()
ssh_host=github-ci@c.quick-lint-js.com
remote_builds_root=/var/www/c.quick-lint-js.com/builds
remote_website_staging_root=/var/www/quick-lint-js.com
remote_website_deploy_symlink=/var/www/quick-lint-js.com/www

main() {
  if [ "${#}" -ne 1 ]; then
    printf 'error: missing commit hash\n' >&2
    return 1
  fi
  commit_hash="${1}"

  remote_website_staging_dir="${remote_website_staging_root}/website-${commit_hash}"
  remote_website_zip="${remote_builds_root}/${commit_hash}/website/website.zip"

  ssh "${ssh_args[@]}" "${ssh_host}" -- sh -sx "${remote_website_staging_dir}" "${remote_website_zip}" <<'EOF'
    set -e
    set -u
    website_staging_dir="${1}"
    website_zip="${2}"

    rm -rf "${website_staging_dir}/"
    mkdir -p "${website_staging_dir}/"
    unzip -d "${website_staging_dir}/" "${website_zip}"
EOF

  ssh "${ssh_args[@]}" "${ssh_host}" -- sh -sx "${remote_website_staging_dir}" "${remote_website_deploy_symlink}" <<'EOF'
    set -e
    set -u
    website_staging_dir="${1}"
    website_deploy_symlink="${2}"

    ln --force --no-dereference --symbolic "${website_staging_dir}/www" "${website_deploy_symlink}"
EOF
}

main "${@}"

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
