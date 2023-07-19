#!/usr/bin/env python3

# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

import argparse
import pathlib
import re
import typing


def main() -> None:
    test()

    arg_parser = argparse.ArgumentParser()
    arg_parser.add_argument("file")
    args = arg_parser.parse_args()

    check_file(path=pathlib.Path(args.file))


def check_file(path: pathlib.Path) -> None:
    source_code = path.read_text()
    tests = parse_tests(source_code)
    for test in sorted(tests, key=lambda t: -t.body_line_count):
        print(f"{test.full_name}: {test.body_line_count}")


class ParsedTest(typing.NamedTuple):
    suite: str
    name: str
    body: str

    @property
    def full_name(self) -> str:
        return f"{self.suite}.{self.name}"

    @property
    def body_line_count(self) -> int:
        return self.body.count("\n")


def parse_tests(source_code: str) -> typing.List[ParsedTest]:
    pattern = re.compile(
        r"""
        ^TEST(_F|_P)?\(
            \s*(?P<Test_Suite>\w+),
            \s*(?P<test_name>\w+)\s*
        \)\s*\{
        (?P<body>.*?)
        \s*
        ^\}
    """,
        re.DOTALL | re.MULTILINE | re.VERBOSE,
    )

    tests = []
    for match in re.finditer(pattern, source_code):
        tests.append(
            ParsedTest(
                suite=match.group("Test_Suite"),
                name=match.group("test_name"),
                body=match.group("body"),
            )
        )
    return tests


def test() -> None:
    tests = parse_tests(
        """
TEST(Test_Suite, hello) {
  // code
}

TEST(Test_Suite_2, hi) {
  // code
  {
    // more code
  }
  // more code
}
"""
    )
    assert len(tests) == 2
    assert tests[0].suite == "Test_Suite"
    assert tests[0].name == "hello"
    assert tests[0].body_line_count == 1
    assert tests[1].suite == "Test_Suite_2"
    assert tests[1].name == "hi"
    assert tests[1].body_line_count == 5


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
