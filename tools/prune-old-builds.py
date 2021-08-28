#!/usr/bin/python3

# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

import re
import os
import sys
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


def get_commits(repo_name: str, repository_url: str) -> list:
    try:
        return subprocess.check_output(
            f"cd {repo_name} \
            && git fetch --prune {repository_url} '+refs/pull/*/head:refs/remotes/github-pr/*' \
            && git rev-list --all --remotes", shell=True
        ).decode('utf-8').split('\n')
    except subprocess.CalledProcessError as err:
        error_print(err.output.decode())
        exit(err.returncode)


def has_valid_name(folder_name: str) -> bool:
    return not re.match(r'^[a-f0-9]{40}$', folder_name) == None


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
    parser.add_argument("-f", "--delete", action="store_true",
                        help="Delete folders.")
    args, _ = parser.parse_known_args()

    builds_path = Path(args.builds_path[0])
    # https://github.com/quick-lint/quick-lint-js.git
    repository_url = args.repository_url[0]
    repo_name = 'quick-lint-js'

    if not os.path.exists(builds_path):
        error_print(
            f"error: The directory \033[1m{builds_path}\033[0m does not exists.")
        exit(1)

    if not os.path.exists(repo_name):
        error_print("Cloning repository")
        subprocess.check_call(
            ['git', 'clone', '-q', '--bare', repository_url, repo_name])

    error_print('Getting all commits of the repository\n')
    commits = get_commits(repo_name, repository_url)

    to_be_deleted = []
    for folder in os.listdir(builds_path):
        folder_path = builds_path / folder
        created_at = datetime.fromtimestamp(os.path.getctime(folder_path))

        if not folder in commits and has_valid_name(folder) and how_old(created_at) > FOURTEEN_DAYS:
            error_print(
                f"""Build \033[93m{folder}\033[0m accomplish the following criteria:\n* Is not part of the repo/PR\n* Has more then fourteen days\n\033[1;31mThis build will be deleted.\033[0m\n""")

            to_be_deleted.append(folder_path)

    if args.delete:
        for folder_path in to_be_deleted:
            try:
                shutil.rmtree(folder_path)
            except FileNotFoundError:
                error_print("Build folder not found")
                exit(2)
            except Exception as err:
                error_print(err)
                exit(1)

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
