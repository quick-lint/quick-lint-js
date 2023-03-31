#!/usr/bin/env python3

# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

import argparse
import collect_binary_sizes
import pathlib
import sys
import typing


def main() -> None:
    parser = argparse.ArgumentParser(__doc__)
    parser.add_argument("before_build", metavar="PATH", type=pathlib.Path)
    parser.add_argument("after_build", metavar="PATH", type=pathlib.Path)
    args = parser.parse_args()

    before_exe_sizes = get_exe_sizes(args.before_build)
    after_exe_sizes = get_exe_sizes(args.after_build)
    common_exe_names = set(before_exe_sizes.keys()) & set(after_exe_sizes.keys())

    column_headers = ["file", "before", "after", "change"]
    column_widths = [len(header) for header in column_headers]
    table = []
    for exe_name in sorted(common_exe_names):
        before_size = before_exe_sizes[exe_name]
        after_size = after_exe_sizes[exe_name]
        row = [
            exe_name,
            str(before_size),
            str(after_size),
            f"{round((after_size / before_size - 1)*100)}%",
        ]
        for column_index, cell in enumerate(row):
            column_widths[column_index] = max(column_widths[column_index], len(cell))
        table.append(row)

    def print_row(row) -> None:
        for column_index, cell in enumerate(row):
            column_width = column_widths[column_index]
            if column_index == 0:
                sys.stdout.write(" ")
            else:
                sys.stdout.write(" | ")
            is_number = column_index > 0
            if is_number:
                sys.stdout.write(f"{cell:>{column_width}}")
            else:
                sys.stdout.write(f"{cell:<{column_width}}")
        sys.stdout.write("\n")

    print_row(column_headers)
    for column_index, column_width in enumerate(column_widths):
        if column_index > 0:
            sys.stdout.write("+")
        true_column_width = column_width + 2
        sys.stdout.write("-" * true_column_width)
    sys.stdout.write("\n")

    for row in table:
        print_row(row)


def get_exe_sizes(directory: pathlib.Path) -> typing.Mapping[str, int]:
    sizes = collect_binary_sizes.Sizes()
    collect_binary_sizes.FileOrDirectoryRecorder(
        name=(),
        path=directory,
    ).record(sizes=sizes)

    exe_sizes = {}
    for entry in sizes.to_json_like():
        if entry["type"] == "exe":
            exe_sizes[tidy_exe_name(entry["name"])] = entry["size"]
    return exe_sizes


def tidy_exe_name(full_name: typing.Tuple[str, ...]) -> str:
    def tidy_exe_name_part(part: str) -> str:
        for ugliness in (
            "bin/",
            "debian/",
            "demo/",
            "dist/",
            "extension/",
            "package/",
            "quick-lint-js-",
            "quick-lint-js_",
            "usr/",
            "vscode-node_",
            "www/",
        ):
            part = part.replace(ugliness, "")
        return part

    return "::".join(tidy_exe_name_part(part) for part in full_name)


if __name__ == "__main__":
    main()

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
