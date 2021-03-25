#!/usr/bin/env python3

# Copyright (C) 2020  Matthew Glazar
# See end of file for extended copyright information.

import os
import pathlib
import subprocess
import tempfile
import unittest


def get_quick_lint_js_executable_path() -> str:
    variable_name = "QUICK_LINT_JS_EXE"
    path = os.environ.get(variable_name, None)
    if path is None or path == "":
        raise Exception(
            f"Could not determine path to quick-lint-js. Set the {variable_name} environment variable to its path."
        )
    return path


class TestQuickLintJSCLI(unittest.TestCase):
    def test_no_files_fails(self) -> None:
        result = subprocess.run(
            [get_quick_lint_js_executable_path()], capture_output=True
        )
        self.assertEqual(result.returncode, 1)

    def test_good_file_lints_ok(self) -> None:
        with tempfile.TemporaryDirectory() as test_directory:
            test_file = pathlib.Path(test_directory) / "test.js"
            test_file.write_text("console.log('hello world');\n")

            result = subprocess.run(
                [get_quick_lint_js_executable_path(), str(test_file)],
                capture_output=True,
            )
            self.assertEqual(result.returncode, 0)

    def test_file_with_syntax_errors_fails(self) -> None:
        with tempfile.TemporaryDirectory() as test_directory:
            test_file = pathlib.Path(test_directory) / "test.js"
            test_file.write_text("var parenthesesMissing;\nif parenthesesMissing { }\n")

            result = subprocess.run(
                [get_quick_lint_js_executable_path(), str(test_file)],
                capture_output=True,
                encoding="utf-8",
            )
            self.assertEqual(result.returncode, 1)
            self.assertIn("[E017]", result.stderr)


if __name__ == "__main__":
    unittest.main()

# quick-lint-js finds bugs in JavaScript programs.
# Copyright (C) 2020  Matthew Glazar
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
