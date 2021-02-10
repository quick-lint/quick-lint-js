#!/usr/bin/env python3

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

import fnmatch
import pathlib
import pipes
import signal
import subprocess
import sys
import typing
import unittest


def run_quick_lint_js(
    quick_lint_js_executable: pathlib.Path, js_file: pathlib.Path
) -> "LintResult":
    try:
        result = subprocess.run(
            [quick_lint_js_executable, "--", js_file],
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            timeout=10,
        )
    except subprocess.TimeoutExpired as e:
        result = subprocess.CompletedProcess(
            args=e.cmd, returncode=-signal.SIGALRM, stdout=e.stdout, stderr=e.stderr
        )
        # Fall through.
    return LintResult(js_file=js_file, result=result)


class LintResult(typing.NamedTuple):
    js_file: pathlib.Path
    result: subprocess.CompletedProcess

    @property
    def crashed(self) -> bool:
        return self.result.returncode not in (0, 1)

    @property
    def user_runnable_command(self) -> str:
        return " ".join(pipes.quote(str(arg)) for arg in self.result.args)

    def dump(self) -> None:
        sys.stderr.write(f"error: command crashed: {self.user_runnable_command}\n")
        sys.stderr.buffer.write(self.result.stdout)
        sys.stderr.write(f"\nContents of {self.js_file}:\n")
        sys.stderr.buffer.write(self.js_file.read_bytes())


class TestMatchPath(unittest.TestCase):
    def test_match_just_file_name(self) -> None:
        self.assertTrue(
            match_path(
                pattern="file.js",
                path=pathlib.PosixPath("/path/to/file.js"),
            )
        )
        self.assertFalse(
            match_path(
                pattern="notfile.js",
                path=pathlib.PosixPath("/path/to/file.js"),
            )
        )
        self.assertFalse(
            match_path(
                pattern="e.js",
                path=pathlib.PosixPath("/path/to/file.js"),
            )
        )

    def test_match_with_file_name_pattern_ignores_directory_names(self) -> None:
        self.assertFalse(
            match_path(
                pattern="path",
                path=pathlib.PosixPath("/path/to/file.js"),
            )
        )
        self.assertFalse(
            match_path(
                pattern="to",
                path=pathlib.PosixPath("/path/to/file.js"),
            )
        )
        self.assertFalse(
            match_path(
                pattern="/",
                path=pathlib.PosixPath("/path/to/file.js"),
            )
        )

    def test_match_file_name_and_parent(self) -> None:
        self.assertTrue(
            match_path(
                pattern="to/file.js",
                path=pathlib.PosixPath("/path/to/file.js"),
            )
        )

        self.assertFalse(
            match_path(
                pattern="other/file.js",
                path=pathlib.PosixPath("/path/to/file.js"),
            )
        )
        self.assertFalse(
            match_path(
                pattern="o/file.js",
                path=pathlib.PosixPath("/path/to/file.js"),
            )
        )

        self.assertFalse(
            match_path(
                pattern="to/file",
                path=pathlib.PosixPath("/path/to/file.js"),
            )
        )

    def test_match_file_glob_with_parent_directory(self) -> None:
        self.assertTrue(
            match_path(
                pattern="to/*",
                path=pathlib.PosixPath("/path/to/file.js"),
            )
        )

    def test_realistic(self) -> None:
        self.assertTrue(
            match_path(
                pattern="comment/migrated_0036.js",
                path=pathlib.PosixPath(
                    "/home/strager/tmp/Projects/esprima/test/fixtures/comment/migrated_0036.js"
                ),
            )
        )
        self.assertFalse(
            match_path(
                pattern="comment/migrated_0036.js",
                path=pathlib.PosixPath(
                    "/home/strager/tmp/Projects/esprima/test/fixtures/expression/primary/object/migrated_0036.js"
                ),
            )
        )
        self.assertTrue(
            match_path(
                pattern="expression/primary/object/migrated_0036.js",
                path=pathlib.PosixPath(
                    "/home/strager/tmp/Projects/esprima/test/fixtures/expression/primary/object/migrated_0036.js"
                ),
            )
        )


def match_path(pattern: str, path) -> bool:
    Path = path.__class__
    try:
        pattern_parts = _pattern_cache[(Path, pattern)]
    except KeyError:
        pattern_parts = Path(pattern).parts
        _pattern_cache[(Path, pattern)] = pattern_parts

    return all(
        fnmatch.fnmatchcase(part, pattern)
        for (pattern, part) in zip(pattern_parts, path.parts[-len(pattern_parts) :])
    )


_pattern_cache = {}

if __name__ == "__main__":
    unittest.main()
