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

import argparse
import itertools
import pathlib
import re
import sys
import typing
import unittest


class ErrorType(typing.NamedTuple):
    name: str
    arguments: typing.Tuple[typing.Tuple[str, str], ...]


def main() -> None:
    arg_parser = argparse.ArgumentParser()
    arg_parser.add_argument("error_name")
    arg_parser.add_argument("arguments", nargs="*")
    args = arg_parser.parse_args()

    error_name = args.error_name
    raw_arguments = args.arguments
    if len(raw_arguments) % 2 != 0:
        sys.stderr.write(
            f"error: missing name of last argument with type {raw_arguments[-1]}"
        )
    arguments = tuple(zip(raw_arguments[::2], raw_arguments[1::2]))

    error_type = ErrorType(name=error_name, arguments=arguments)

    project_directory = pathlib.Path(__file__).parent / ".."
    source_file_paths = tuple(
        itertools.chain(
            project_directory.glob("src/**/*.cpp"),
            project_directory.glob("src/**/*.h"),
            project_directory.glob("test/**/*.cpp"),
            project_directory.glob("test/**/*.h"),
        )
    )
    for path in source_file_paths:
        source = path.read_text()
        source = modify_source(source, error_type)
        path.write_text(source)


def modify_source(source: str, error_type: ErrorType) -> None:
    modifier = SourceModifier(error_type=error_type, lines=iter(source.splitlines()))
    modifier.process()
    modified_source = "\n".join(modifier._out_lines)
    if source.endswith("\n") and not modified_source.endswith("\n"):
        modified_source += "\n"
    return modified_source


class SourceModifier:
    def __init__(self, error_type: ErrorType, lines: typing.Iterator[str]) -> None:
        self._error_type = error_type
        self._in_lines = lines
        self._out_lines = []

    def take_line(self) -> None:
        try:
            return next(self._in_lines)
        except StopIteration:
            return None

    def process(self) -> None:
        self.parse_top_level()

    def parse_top_level(self) -> None:
        inserted_error_reporter_function_implementation = False

        def append_error_reporter_implementation_if_needed(class_name: str) -> None:
            nonlocal inserted_error_reporter_function_implementation
            if class_name != "error_collector":
                if not inserted_error_reporter_function_implementation:
                    self.append_error_reporter_implementation(class_name=class_name)
                    inserted_error_reporter_function_implementation = True

        inserted_error_reporter_test = False

        def append_error_reporter_test_if_needed(fixture_name: str) -> None:
            nonlocal inserted_error_reporter_test
            if not inserted_error_reporter_test:
                self.append_error_reporter_test(fixture_name=fixture_name)
                inserted_error_reporter_test = True

        inserted_error_collector_error_print_to_case = False

        def append_error_collector_error_print_to_case() -> None:
            nonlocal inserted_error_collector_error_print_to_case
            if not inserted_error_collector_error_print_to_case:
                self._out_lines.append(f"    QLJS_CASE(error_{self._error_type.name})")
                inserted_error_collector_error_print_to_case = True

        while True:
            line = self.take_line()
            if line is None:
                return

            buffered_lines = [line]

            match = re.match(
                r"\s*(class|struct) (?P<class>(\S+_)?error_reporter|error_collector).*",
                line,
            )
            if match is not None:
                self._out_lines.extend(buffered_lines)
                self.parse_in_error_reporter_class(class_name=match.group("class"))
                continue

            match = re.match(r"\s*void PrintTo\(.*error_collector::error.*", line)
            if match is not None:
                self._out_lines.extend(buffered_lines)
                while True:
                    line = self.take_line()
                    if line is None:
                        return
                    match = re.match(r"\s*QLJS_CASE\(error_(?P<name>\S+)\)$", line)
                    if match is not None:
                        current_name = match.group("name")
                        if current_name > self._error_type.name:
                            append_error_collector_error_print_to_case()
                        self._out_lines.append(line)
                        continue
                    elif line == "  }":
                        append_error_collector_error_print_to_case()
                        self._out_lines.append(line)
                        break
                    else:
                        self._out_lines.append(line)
                        continue
                continue

            match = re.match(
                r"\s*TEST_F\((?P<fixture>test_\S+_error_reporter), (?P<name>\S+)\).*",
                line,
            )
            if match is not None:
                test_name = match.group("name")
                if test_name > self._error_type.name:
                    append_error_reporter_test_if_needed(
                        fixture_name=match.group("fixture")
                    )
                self._out_lines.extend(buffered_lines)
                while True:
                    line = self.take_line()
                    if line is None:
                        return
                    self._out_lines.append(line)
                    if line == "}":
                        line = self.take_line()
                        if line is None:
                            return
                        if line == "}":
                            self._out_lines.append("")
                            append_error_reporter_test_if_needed(
                                fixture_name=match.group("fixture")
                            )
                            self._out_lines[-1:] = []  # Remove trailing blank line.
                        self._out_lines.append(line)
                        break
                continue

            match = re.match(
                r"\s*void (?P<class>.*)::report_fatal_error_unimplemented_token\(.*",
                line,
            )
            if match is not None:
                append_error_reporter_implementation_if_needed(
                    class_name=match.group("class")
                )
                self._out_lines.extend(buffered_lines)
                continue

            class_name = None
            function_name = None

            one_line_match = re.match(
                r"\s*void (?P<class>.*)::report_error_(?P<name>[^(]+)\(.*", line
            )
            split_match = re.match(r"\s*void (?P<class>.*)::$", line)
            if one_line_match is not None:
                class_name = one_line_match.group("class")
                function_name = one_line_match.group("name")
            elif split_match is not None:
                # Parse function implementation header split across multiple lines:
                # void foo::
                #     bar() {
                class_name = split_match.group("class")
                second_line = self.take_line()
                if second_line is not None:
                    buffered_lines.append(second_line)
                    match = re.match(
                        r"\s*report_error_(?P<name>[^(]+)\(.*", second_line
                    )
                    if match is not None:
                        function_name = match.group("name")

            if class_name is not None and function_name is not None:
                if function_name > self._error_type.name:
                    append_error_reporter_implementation_if_needed(
                        class_name=class_name
                    )
                self._out_lines.extend(buffered_lines)
                continue

            self._out_lines.extend(buffered_lines)

    def parse_in_error_reporter_class(self, class_name: str) -> None:
        # Skip until we see an error_reporter function declaration.
        while True:
            line = self.take_line()
            if line is None:
                return
            if self.match_error_reporter_function_declaration(line) is not None:
                break
            self._out_lines.append(line)
            if line == "};":
                return

        inserted_error_reporter_function_declaration = False

        def append_derived_error_reporter_function_declaration_if_needed() -> None:
            nonlocal inserted_error_reporter_function_declaration
            if not inserted_error_reporter_function_declaration:
                self.append_derived_error_reporter_function_declaration(
                    class_name=class_name
                )
                inserted_error_reporter_function_declaration = True

        while True:
            if line == "};":
                append_derived_error_reporter_function_declaration_if_needed()
                self._out_lines.append(line)
                return
            elif line == "" and class_name != "error_collector":
                append_derived_error_reporter_function_declaration_if_needed()
                self._out_lines.append(line)
            elif "enum error_kind " in line and class_name == "error_collector":
                append_derived_error_reporter_function_declaration_if_needed()
                self._out_lines.append(line)
                self.parse_in_error_collector_error_kind_enum()
                return
            else:
                current_name = self.match_error_reporter_function_declaration(line)
                if current_name is not None:
                    if current_name > self._error_type.name:
                        append_derived_error_reporter_function_declaration_if_needed()
                self._out_lines.append(line)

            line = self.take_line()
            if line is None:
                return

    def parameter_list(self, include_parameter_names: bool = True) -> str:
        def parameter(parameter: typing.Tuple[str, str]) -> str:
            (param_type, param_name) = parameter
            if include_parameter_names:
                return f"{param_type} {param_name}"
            else:
                return f"{param_type}"

        return ", ".join(parameter(p) for p in self._error_type.arguments)

    def append_error_reporter_implementation(self, class_name: str) -> None:
        parameters = self.parameter_list()
        self._out_lines.append(
            f"void {class_name}::report_error_{self._error_type.name}({parameters}) {{"
        )
        if class_name == "text_error_reporter":
            if self._error_type.arguments:
                for (i, (param_type, param_name)) in enumerate(
                    self._error_type.arguments
                ):
                    if param_type in ("identifier", "source_code_span"):
                        self._out_lines.append(f"  this->log_location({param_name});")
                        severity = "error" if i == 0 else "note"
                        self._out_lines.append(
                            f'  this->output_ << "{severity}: TODO\\n";'
                        )
            else:
                self._out_lines.append(f'  this->output_ << "error: TODO\\n";')
        elif class_name == "vim_qflist_json_error_reporter":
            for (_param_type, param_name) in self._error_type.arguments:
                self._out_lines.append(
                    f"  this->write_qflist_entry_header({param_name});"
                )
                break
            self._out_lines.append(f'  this->output_ << ", \\"text\\": \\"TODO\\"}}";')
        self._out_lines.append(f"}}")
        self._out_lines.append(f"")

    def append_error_reporter_test(self, fixture_name: str) -> None:
        self._out_lines.append(f"TEST_F({fixture_name}, {self._error_type.name}) {{")
        self._out_lines.append(f'  padded_string input(u8"TODO");')
        self._out_lines.append(f"")
        if fixture_name == "test_vim_qflist_json_error_reporter":
            self._out_lines.append(f"  vim_qflist_json_error_reporter reporter =")
            self._out_lines.append(
                f"      this->make_reporter(&input, /*vim_bufnr=*/0);"
            )
            self._out_lines.append(
                f"  reporter.report_error_{self._error_type.name}();"
            )
            self._out_lines.append(f"  reporter.finish();")
            self._out_lines.append(f"")
            self._out_lines.append(
                f'  ::Json::Value qflist = this->parse_json()["qflist"];'
            )
            self._out_lines.append(f"  ASSERT_EQ(qflist.size(), 1);")
            self._out_lines.append(f'  EXPECT_EQ(qflist[0]["col"], 1 /* TODO */);')
            self._out_lines.append(f'  EXPECT_EQ(qflist[0]["end_col"], 1 /* TODO */);')
            self._out_lines.append(f'  EXPECT_EQ(qflist[0]["end_lnum"], 1 /* TODO */);')
            self._out_lines.append(f'  EXPECT_EQ(qflist[0]["lnum"], 1 /* TODO */);')
            self._out_lines.append(f'  EXPECT_EQ(qflist[0]["text"], "TODO");')
        else:
            self._out_lines.append(
                f"  this->make_reporter(&input).report_error_{self._error_type.name}();"
            )
            self._out_lines.append(
                f'  EXPECT_EQ(this->get_output(), "FILE:TODO:TODO: error: TODO\\n");'
            )
        self._out_lines.append(f"}}")
        self._out_lines.append(f"")

    def append_derived_error_reporter_function_declaration(
        self, class_name: str
    ) -> None:
        if class_name == "error_reporter":
            parameters = self.parameter_list()
            self._out_lines.append(
                f"  virtual void report_error_{self._error_type.name}({parameters}) = 0;"
            )
        else:
            parameters = self.parameter_list(
                include_parameter_names=class_name != "null_error_reporter"
            )
            head = f"void report_error_{self._error_type.name}({parameters}) override"
            if class_name == "null_error_reporter":
                self._out_lines.append(f"  {head} {{}}")
            elif class_name == "error_collector":

                def argument(parameter: typing.Tuple[str, str]) -> str:
                    (param_type, param_name) = parameter
                    return param_name

                self._out_lines.append(f"  {head} {{")
                arguments = ", ".join(
                    (f"error_{self._error_type.name}",)
                    + tuple(argument(p) for p in self._error_type.arguments)
                )
                self._out_lines.append(f"    this->errors.emplace_back({arguments});")
                self._out_lines.append(f"  }}")
                self._out_lines.append(f"")
            else:
                self._out_lines.append(f"  {head};")

    def parse_in_error_collector_error_kind_enum(self) -> None:
        inserted_error_kind_enum = False

        def append_error_collector_error_kind_enum_if_needed() -> None:
            nonlocal inserted_error_kind_enum
            if not inserted_error_kind_enum:
                self._out_lines.append(f"    error_{self._error_type.name},")
                inserted_error_kind_enum = True

        while True:
            line = self.take_line()
            if line is None:
                return
            match = re.match(r"\s*error_(?P<name>[^,]+),$", line)
            if match is not None:
                current_name = match.group("name")
                if current_name > self._error_type.name:
                    append_error_collector_error_kind_enum_if_needed()
                self._out_lines.append(line)
            elif "};" in line:
                append_error_collector_error_kind_enum_if_needed()
                self._out_lines.append(line)
                return
            else:
                self._out_lines.append(line)

    def match_error_reporter_function_declaration(
        self, line: str
    ) -> typing.Optional[str]:
        match = re.match(r"\s*(virtual )?void report_error_(?P<name>[^(]+)\(.*", line)
        if match is None:
            return None
        else:
            return match.group("name")


if __name__ == "__main__":
    main()
