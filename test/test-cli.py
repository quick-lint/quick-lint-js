#!/usr/bin/env python3

# Copyright (C) 2020  Matthew "strager" Glazar
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

    def test_file_with_syntax_errors_with_non_matching_exit_fail_on_does_not_fail(
        self,
    ) -> None:
        with tempfile.TemporaryDirectory() as test_directory:
            test_file = pathlib.Path(test_directory) / "test.js"
            test_file.write_text("var parenthesesMissing;\nif parenthesesMissing { }\n")

            result = subprocess.run(
                [
                    get_quick_lint_js_executable_path(),
                    "--exit-fail-on=E057",
                    str(test_file),
                ],
                capture_output=True,
                encoding="utf-8",
            )
            self.assertEqual(result.returncode, 0)
            self.assertIn("[E017]", result.stderr)  # Error should be printed

    def test_single_config_file(self) -> None:
        with tempfile.TemporaryDirectory() as test_directory:
            test_file = pathlib.Path(test_directory) / "test.js"
            test_file.write_text("console.log(myGlobalVariable);")

            config_file = pathlib.Path(test_directory) / "config.json"
            config_file.write_text('{"globals":{"myGlobalVariable": true}}')

            result = subprocess.run(
                [
                    get_quick_lint_js_executable_path(),
                    "--config-file",
                    str(config_file),
                    str(test_file),
                ],
                capture_output=True,
                encoding="utf-8",
            )
            self.assertEqual(result.stderr, "")
            self.assertEqual(result.stdout, "")
            self.assertEqual(result.returncode, 0)

    def test_missing_explicit_config_file(self) -> None:
        with tempfile.TemporaryDirectory() as test_directory:
            test_file = pathlib.Path(test_directory) / "test.js"
            test_file.write_text("console.log(myGlobalVariable);")

            config_file = pathlib.Path(test_directory) / "config.json"

            result = subprocess.run(
                [
                    get_quick_lint_js_executable_path(),
                    "--config-file",
                    str(config_file),
                    str(test_file),
                ],
                capture_output=True,
                encoding="utf-8",
            )
            self.assertIn("config.json", result.stderr)
            self.assertIn("error:", result.stderr)
            self.assertEqual(result.returncode, 1)

    def test_automatically_find_config_file(self) -> None:
        for config_file_name in ("quick-lint-js.config", ".quick-lint-js.config"):
            with tempfile.TemporaryDirectory() as test_directory:
                test_file = pathlib.Path(test_directory) / "test.js"
                test_file.write_text("console.log(myGlobalVariable);")

                config_file = pathlib.Path(test_directory) / config_file_name
                config_file.write_text('{"globals":{"myGlobalVariable": true}}')

                result = subprocess.run(
                    [
                        get_quick_lint_js_executable_path(),
                        str(test_file),
                    ],
                    capture_output=True,
                    encoding="utf-8",
                )
                self.assertEqual(result.stderr, "")
                self.assertEqual(result.stdout, "")
                self.assertEqual(result.returncode, 0)

    def test_config_file_parse_error_prevents_lint(self) -> None:
        with tempfile.TemporaryDirectory() as test_directory:
            test_file = pathlib.Path(test_directory) / "test.js"
            test_file.write_text("console.log(myGlobalVariable);")

            config_file = pathlib.Path(test_directory) / "quick-lint-js.config"
            config_file.write_text('INVALID JSON')

            result = subprocess.run(
                [
                    get_quick_lint_js_executable_path(),
                    str(test_file),
                ],
                capture_output=True,
                encoding="utf-8",
            )
            self.assertEqual(result.returncode, 1)

            # test.js shouldn't be linted.
            self.assertNotIn("myGlobalVariable", result.stderr)
            self.assertNotIn("E057", result.stderr)

            # quick-lint-js.config should have errors.
            self.assertIn("quick-lint-js.config", result.stderr)
            self.assertIn("E164", result.stderr)

    def test_config_error_for_multiple_js_files_is_printed_only_once(self) -> None:
        with tempfile.TemporaryDirectory() as test_directory:
            test_file_1 = pathlib.Path(test_directory) / "test1.js"
            test_file_1.write_text("")
            test_file_2 = pathlib.Path(test_directory) / "test2.js"
            test_file_2.write_text("")

            config_file = pathlib.Path(test_directory) / "quick-lint-js.config"
            config_file.write_text('INVALID JSON')

            result = subprocess.run(
                [
                    get_quick_lint_js_executable_path(),
                    str(test_file_1),
                    str(test_file_2),
                ],
                capture_output=True,
                encoding="utf-8",
            )
            self.assertEqual(result.returncode, 1)
            self.assertEqual(result.stderr.count("E164"), 1)

    def test_errors_for_all_config_files_are_printed(self) -> None:
        with tempfile.TemporaryDirectory() as test_directory:
            test_dir_1 = pathlib.Path(test_directory) / "dir1"
            test_dir_1.mkdir()
            test_file_1 = test_dir_1 / "test.js"
            test_file_1.write_text("")
            config_file_1 = test_dir_1 / "quick-lint-js.config"
            config_file_1.write_text('INVALID JSON')

            test_dir_2 = pathlib.Path(test_directory) / "dir2"
            test_dir_2.mkdir()
            test_file_2 = test_dir_2 / "test.js"
            test_file_2.write_text("")
            config_file_2 = test_dir_2 / "quick-lint-js.config"
            config_file_2.write_text('INVALID JSON')

            result = subprocess.run(
                [
                    get_quick_lint_js_executable_path(),
                    str(test_file_1),
                    str(test_file_2),
                ],
                capture_output=True,
                encoding="utf-8",
            )
            self.assertEqual(result.returncode, 1)
            self.assertIn("dir1", result.stderr)
            self.assertIn("dir2", result.stderr)
            self.assertEqual(result.stderr.count("E164"), 2)


if __name__ == "__main__":
    unittest.main()

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
