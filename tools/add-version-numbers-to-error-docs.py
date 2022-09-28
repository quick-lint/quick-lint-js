#!/usr/bin/env python3

# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

import collections
import pathlib
import re
import subprocess
import typing

ErrorCode = str
Version = str

all_versions: typing.List[Version] = sorted(
    subprocess.check_output(["git", "tag"], encoding="utf-8").splitlines()
)

error_code_file_paths = [
    "src/quick-lint-js/error.h",
    "src/quick-lint-js/diagnostic-types.h",
    "src/quick-lint-js/fe/diagnostic-types.h",
]


def get_error_codes_from_file(file_content: str) -> typing.Set[ErrorCode]:
    matches = re.findall(r'"(E[0-9]+)"', file_content)
    return set(matches)


def get_error_codes_in_version(version: Version) -> typing.Set[ErrorCode]:
    error_codes = set()
    for error_code_file_path in error_code_file_paths:
        try:
            error_code_file_content = subprocess.check_output(
                ["git", "show", f"{version}:{error_code_file_path}"], encoding="utf-8"
            )
            error_codes |= get_error_codes_from_file(error_code_file_content)
        except subprocess.CalledProcessError:
            # The file doesn't exist at this version.
            pass
    return error_codes


error_code_versions: typing.Mapping[
    ErrorCode, typing.List[Version]
] = collections.defaultdict(list)
for version in sorted(all_versions):
    for error_code in get_error_codes_in_version(version):
        if len(error_code) == 4:
            error_code = error_code[0] + "0" + error_code[1:]
        error_code_versions[error_code].append(version)

for p in pathlib.Path("docs/errors").glob("*.md"):
    error_code = p.name.replace(".md", "")
    versions = error_code_versions[error_code]
    if versions:
        introduced_version = versions[0]

        md = p.read_text()
        if "Introduced in quick-lint-js version" not in md:
            md += f"\nIntroduced in quick-lint-js version {introduced_version}.\n"
            p.write_text(md)

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
