#!/usr/bin/python3

# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

import os
import shutil
import argparse
import subprocess
from pathlib import Path
from datetime import datetime


FOURTEEN_DAYS = 14


def how_old(date: datetime) -> int:
    """Return a integer that represents how many days has passed given a date"""
    return (datetime.now() - date).days


def error_print(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)


if __name__ == '__main__':
    parser = argparse.ArgumentParser("Prune old builds")
    parser.add_argument(
        "builds_path",
        help="The path where the builds are stored.",
        nargs=1,
        type=str)
    parser.add_argument(
        "repository_url",
        help="The url to the repository.",
        nargs=1,
        type=str)
    args, _ = parser.parse_known_args()

    builds_path = Path(args.builds_path[0])
    # https://github.com/quick-lint/quick-lint-js.git
    repository_url = args.repository_url[0]
    repo_name = 'quick-lint-js'

    if not os.path.exists(builds_path):
        print(
            f"error: The directory \033[1m{builds_path}\033[0m does not exists.")
        exit(1)

    if not os.path.exists(repo_name):
        print("Cloning repository")
        subprocess.run(['git', 'clone', '-q', repository_url])

    print('Getting all commits of the repository\n')
    commits = subprocess.getoutput(
        f"cd {repo_name} \
        && git pull origin master \
        && git fetch origin 'refs/pull/*/head:refs/remotes/github-pr/*' \
        && git rev-list --all --remotes"
    ).split('\n')

    for folder in os.listdir(builds_path):
        folder_path = builds_path / folder
        created_at = datetime.fromtimestamp(os.path.getctime(folder_path))

        if not folder in commits and how_old(created_at) > FOURTEEN_DAYS:
            print(
                f"""Build \033[93m{folder}\033[0m accomplish the following criteria:\n* Is not part of the repo/PR\n* Has more then fourteen days\n\033[1;31mThis build will be deleted.\033[0m\n""")
            try:
                shutil.rmtree(folder_path)
            except OSError as e:
                print("Error: %s : %s" % (folder_path, e.strerror))

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
