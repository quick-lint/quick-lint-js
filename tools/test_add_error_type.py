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

from add_error_type import ErrorType, modify_source
import unittest


class TestAddErrorReporterFunction(unittest.TestCase):
    original_source = """\
class error_reporter {
 public:
  error_reporter() noexcept = default;

  virtual ~error_reporter() = default;

  virtual void report_error_first(identifier assignment) = 0;
  virtual void report_error_second(
      identifier declaration, identifier assignment,
      variable_kind var_kind) = 0;
};
"""

    def test_add_error_type_before_first_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="aaaaa", arguments=())
        )
        self.assertIn(
            """\
  virtual ~error_reporter() = default;

  virtual void report_error_aaaaa() = 0;
  virtual void report_error_first(identifier assignment) = 0;""",
            modified_source,
        )
        self.assertEqual(modified_source.count("report_error_aaaaa"), 1)

    def test_add_error_type_after_last_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="zzzzz", arguments=())
        )
        self.assertIn(
            """\
  virtual void report_error_second(
      identifier declaration, identifier assignment,
      variable_kind var_kind) = 0;
  virtual void report_error_zzzzz() = 0;""",
            modified_source,
        )
        self.assertEqual(modified_source.count("report_error_zzzzz"), 1)

    def test_add_error_type_in_middle_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="mmmmm", arguments=())
        )
        self.assertIn(
            """\
  virtual void report_error_first(identifier assignment) = 0;
  virtual void report_error_mmmmm() = 0;
  virtual void report_error_second(
      identifier declaration, identifier assignment,
      variable_kind var_kind) = 0;""",
            modified_source,
        )
        self.assertEqual(modified_source.count("report_error_mmmmm"), 1)

    def test_single_argument(self) -> None:
        modified_source = modify_source(
            self.original_source,
            ErrorType(name="test", arguments=(("source_code_span", "where"),)),
        )
        self.assertIn(
            "virtual void report_error_test(source_code_span where) = 0;",
            modified_source,
        )


class TestAddNullErrorReporterFunction(unittest.TestCase):
    original_source = """\
class null_error_reporter : public error_reporter {
 public:
  static null_error_reporter instance;

  void report_error_first(identifier) override {}
  void report_error_second(
      identifier, identifier, variable_kind) override {}
};
"""

    def test_add_error_type_before_first_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="aaaaa", arguments=())
        )
        self.assertIn(
            """\
  void report_error_aaaaa() override {}
  void report_error_first(identifier) override {}""",
            modified_source,
        )
        self.assertEqual(modified_source.count("report_error_aaaaa"), 1)

    def test_add_error_type_after_last_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="zzzzz", arguments=())
        )
        self.assertIn(
            """\
  void report_error_second(
      identifier, identifier, variable_kind) override {}
  void report_error_zzzzz() override {}""",
            modified_source,
        )
        self.assertEqual(modified_source.count("report_error_zzzzz"), 1)

    def test_add_error_type_in_middle_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="mmmmm", arguments=())
        )
        self.assertIn(
            """\
  void report_error_first(identifier) override {}
  void report_error_mmmmm() override {}
  void report_error_second(
      identifier, identifier, variable_kind) override {}""",
            modified_source,
        )
        self.assertEqual(modified_source.count("report_error_mmmmm"), 1)

    def test_single_argument(self) -> None:
        modified_source = modify_source(
            self.original_source,
            ErrorType(name="test", arguments=(("source_code_span", "where"),)),
        )
        self.assertIn(
            "void report_error_test(source_code_span) override {}", modified_source
        )


class TestAddTextErrorReporterFunctionDeclaration(unittest.TestCase):
    original_source = """\
class text_error_reporter final : public error_reporter {
 public:
  explicit text_error_reporter(std::ostream &output);

  void set_source(padded_string_view input, const char *file_name);

  void report_error_first(identifier assignment) override;
  void report_error_second(
      identifier declaration, identifier assignment,
      variable_kind var_kind) override;
};
"""

    def test_add_error_type_before_first_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="aaaaa", arguments=())
        )
        self.assertIn(
            """\
  void report_error_aaaaa() override;
  void report_error_first(identifier assignment) override;""",
            modified_source,
        )
        self.assertEqual(modified_source.count("report_error_aaaaa"), 1)

    def test_add_error_type_after_last_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="zzzzz", arguments=())
        )
        self.assertIn(
            """\
  void report_error_second(
      identifier declaration, identifier assignment,
      variable_kind var_kind) override;
  void report_error_zzzzz() override;""",
            modified_source,
        )
        self.assertEqual(modified_source.count("report_error_zzzzz"), 1)

    def test_add_error_type_in_middle_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="mmmmm", arguments=())
        )
        self.assertIn(
            """\
  void report_error_first(identifier assignment) override;
  void report_error_mmmmm() override;
  void report_error_second(
      identifier declaration, identifier assignment,""",
            modified_source,
        )
        self.assertEqual(modified_source.count("report_error_mmmmm"), 1)

    def test_single_argument(self) -> None:
        modified_source = modify_source(
            self.original_source,
            ErrorType(name="test", arguments=(("source_code_span", "where"),)),
        )
        self.assertIn(
            "void report_error_test(source_code_span where) override;", modified_source
        )


class TestManyErrorReporterFunctionDeclarations(unittest.TestCase):
    original_source = """\
class foo_error_reporter final : public error_reporter {
 public:
  void report_error_bad() override; // foo
};

class bar_error_reporter final : public error_reporter {
 public:
  void report_error_bad() override; // bar
};
"""

    def test_add_error_type_before_first_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="aaaaa", arguments=())
        )
        self.assertIn(
            """\
  void report_error_aaaaa() override;
  void report_error_bad() override; // foo""",
            modified_source,
        )
        self.assertIn(
            """\
  void report_error_aaaaa() override;
  void report_error_bad() override; // bar""",
            modified_source,
        )
        self.assertEqual(modified_source.count("report_error_aaaaa"), 2)


class TestAddTextErrorReporterFunctionImplementation(unittest.TestCase):
    original_source = """\
void text_error_reporter::report_error_first(source_code_span where) {
  log_location(where);
  this->output_ << "error: first\\n";
}

void text_error_reporter::report_error_second(
  identifier left, identifier right, variable_kind) {
  log_location(left);
  this->output_ << "error: left\\n";
  log_location(right);
  this->output_ << "note: right\\n";
}

void text_error_reporter::report_fatal_error_unimplemented_token(
    const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
    token_type type, const char8 *token_begin) {
"""

    def test_add_error_type_before_first_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="aaaaa", arguments=())
        )
        self.assertIn(
            """\
void text_error_reporter::report_error_aaaaa() {
  this->output_ << "error: TODO\\n";
}

void text_error_reporter::report_error_first(source_code_span where) {""",
            modified_source,
        )
        self.assertEqual(modified_source.count("report_error_aaaaa"), 1)

    def test_add_error_type_after_last_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="zzzzz", arguments=())
        )
        self.assertIn(
            """\
  log_location(right);
  this->output_ << "note: right\\n";
}

void text_error_reporter::report_error_zzzzz() {
  this->output_ << "error: TODO\\n";
}

void text_error_reporter::report_fatal_error_unimplemented_token(""",
            modified_source,
        )
        self.assertEqual(modified_source.count("report_error_zzzzz"), 1)

    def test_add_error_type_in_middle_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="mmmmm", arguments=())
        )
        self.assertIn(
            """\
  this->output_ << "error: first\\n";
}

void text_error_reporter::report_error_mmmmm() {
  this->output_ << "error: TODO\\n";
}

void text_error_reporter::report_error_second(
  identifier left, identifier right, variable_kind) {
  log_location(left);""",
            modified_source,
        )
        self.assertEqual(modified_source.count("report_error_mmmmm"), 1)

    def test_single_argument(self) -> None:
        modified_source = modify_source(
            self.original_source,
            ErrorType(name="test", arguments=(("source_code_span", "where"),)),
        )
        self.assertIn(
            """\
void text_error_reporter::report_error_test(source_code_span where) {
  this->log_location(where);
  this->output_ << "error: TODO\\n";
}
""",
            modified_source,
        )

    def test_multiple_location_arguments(self) -> None:
        modified_source = modify_source(
            self.original_source,
            ErrorType(
                name="test",
                arguments=(("source_code_span", "where"), ("identifier", "other"),),
            ),
        )
        self.assertIn(
            """\
void text_error_reporter::report_error_test(source_code_span where, identifier other) {
  this->log_location(where);
  this->output_ << "error: TODO\\n";
  this->log_location(other);
  this->output_ << "note: TODO\\n";
}
""",
            modified_source,
        )

    def test_multiple_non_location_arguments(self) -> None:
        modified_source = modify_source(
            self.original_source,
            ErrorType(
                name="test",
                arguments=(
                    ("source_code_span", "where"),
                    ("variable_kind", "kind"),
                    ("variable_kind", "other_kind"),
                ),
            ),
        )
        self.assertIn(
            """\
void text_error_reporter::report_error_test(source_code_span where, variable_kind kind, variable_kind other_kind) {
  this->log_location(where);
  this->output_ << "error: TODO\\n";
}
""",
            modified_source,
        )


class TestAddTextErrorReporterTest(unittest.TestCase):
    original_source = """\
namespace quick_lint_js {
namespace {
TEST_F(test_text_error_reporter, first) {
  padded_string input(u8"");
  this->make_reporter(&input).report_error_first();
  EXPECT_EQ(this->get_output(), "FILE:1:1: error: first\\n");
}

TEST_F(test_text_error_reporter, second) {
  padded_string input(u8"");
  this->make_reporter(&input).report_error_second();
  EXPECT_EQ(this->get_output(), "FILE:1:1: error: second\\n");
}
}
}
"""

    def test_add_error_type_before_first_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="aaaaa", arguments=())
        )
        self.assertIn(
            """\
namespace quick_lint_js {
namespace {
TEST_F(test_text_error_reporter, aaaaa) {
  padded_string input(u8"TODO");

  this->make_reporter(&input).report_error_aaaaa();
  EXPECT_EQ(this->get_output(), "FILE:TODO:TODO: error: TODO\\n");
}

TEST_F(test_text_error_reporter, first) {""",
            modified_source,
        )
        self.assertEqual(modified_source.count(".report_error_aaaaa"), 1)
        self.assertEqual(modified_source.count("test_text_error_reporter, aaaaa"), 1)

    def test_add_error_type_after_last_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="zzzzz", arguments=())
        )
        self.assertIn(
            """\
  this->make_reporter(&input).report_error_second();
  EXPECT_EQ(this->get_output(), "FILE:1:1: error: second\\n");
}

TEST_F(test_text_error_reporter, zzzzz) {
  padded_string input(u8"TODO");

  this->make_reporter(&input).report_error_zzzzz();
  EXPECT_EQ(this->get_output(), "FILE:TODO:TODO: error: TODO\\n");
}
}
}
""",
            modified_source,
        )
        self.assertEqual(modified_source.count(".report_error_zzzzz"), 1)
        self.assertEqual(modified_source.count("test_text_error_reporter, zzzzz"), 1)

    def test_add_error_type_in_middle_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="mmmmm", arguments=())
        )
        self.assertIn(
            """\
  this->make_reporter(&input).report_error_first();
  EXPECT_EQ(this->get_output(), "FILE:1:1: error: first\\n");
}

TEST_F(test_text_error_reporter, mmmmm) {
  padded_string input(u8"TODO");

  this->make_reporter(&input).report_error_mmmmm();
  EXPECT_EQ(this->get_output(), "FILE:TODO:TODO: error: TODO\\n");
}

TEST_F(test_text_error_reporter, second) {""",
            modified_source,
        )
        self.assertEqual(modified_source.count(".report_error_mmmmm"), 1)
        self.assertEqual(modified_source.count("test_text_error_reporter, mmmmm"), 1)


class TestAddVimQFListJSONErrorReporterFunctionImplementation(unittest.TestCase):
    original_source = """\
void vim_qflist_json_error_reporter::
    report_error_first(source_code_span where) {
  this->write_qflist_entry_header(where);
  this->output_ << ", \\"text\\": \\"first\\"}";
}

void vim_qflist_json_error_reporter::report_error_second(
  identifier left, identifier, variable_kind) {
  this->write_qflist_entry_header(left);
  this->output_ << ", \\"text\\": \\"left\\"}";
}

void vim_qflist_json_error_reporter::report_fatal_error_unimplemented_token(
    const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
    token_type type, const char8 *token_begin) {
"""

    def test_add_error_type_before_first_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="aaaaa", arguments=())
        )
        self.assertIn(
            """\
void vim_qflist_json_error_reporter::report_error_aaaaa() {
  this->output_ << ", \\"text\\": \\"TODO\\"}";
}

void vim_qflist_json_error_reporter::
    report_error_first(source_code_span where) {""",
            modified_source,
        )
        self.assertEqual(modified_source.count("report_error_aaaaa"), 1)

    def test_add_error_type_after_last_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="zzzzz", arguments=())
        )
        self.assertIn(
            """\
  this->output_ << ", \\"text\\": \\"left\\"}";
}

void vim_qflist_json_error_reporter::report_error_zzzzz() {
  this->output_ << ", \\"text\\": \\"TODO\\"}";
}

void vim_qflist_json_error_reporter::report_fatal_error_unimplemented_token(""",
            modified_source,
        )
        self.assertEqual(modified_source.count("report_error_zzzzz"), 1)

    def test_add_error_type_in_middle_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="mmmmm", arguments=())
        )
        self.assertIn(
            """\
  this->output_ << ", \\"text\\": \\"first\\"}";
}

void vim_qflist_json_error_reporter::report_error_mmmmm() {
  this->output_ << ", \\"text\\": \\"TODO\\"}";
}

void vim_qflist_json_error_reporter::report_error_second(
  identifier left, identifier, variable_kind) {
  this->write_qflist_entry_header(left);""",
            modified_source,
        )
        self.assertEqual(modified_source.count("report_error_mmmmm"), 1)

    def test_single_argument(self) -> None:
        modified_source = modify_source(
            self.original_source,
            ErrorType(name="test", arguments=(("source_code_span", "where"),)),
        )
        self.assertIn(
            """\
void vim_qflist_json_error_reporter::report_error_test(source_code_span where) {
  this->write_qflist_entry_header(where);
  this->output_ << ", \\"text\\": \\"TODO\\"}";
}
""",
            modified_source,
        )


class TestAddVimQFListJSONErrorReporterTest(unittest.TestCase):
    original_source = """\
namespace quick_lint_js {
namespace {
TEST_F(test_vim_qflist_json_error_reporter, first) {
  padded_string input(u8"TODO");
  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report_error_first();
  reporter.finish();
  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["text"], "first");
}

TEST_F(test_vim_qflist_json_error_reporter, second) {
  padded_string input(u8"TODO");
  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report_error_second();
  reporter.finish();
  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["text"], "second");
}
}
}
"""

    def test_add_error_type_before_first_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="aaaaa", arguments=())
        )
        self.assertIn(
            """\
namespace quick_lint_js {
namespace {
TEST_F(test_vim_qflist_json_error_reporter, aaaaa) {
  padded_string input(u8"TODO");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report_error_aaaaa();
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 1 /* TODO */);
  EXPECT_EQ(qflist[0]["end_col"], 1 /* TODO */);
  EXPECT_EQ(qflist[0]["end_lnum"], 1 /* TODO */);
  EXPECT_EQ(qflist[0]["lnum"], 1 /* TODO */);
  EXPECT_EQ(qflist[0]["text"], "TODO");
}

TEST_F(test_vim_qflist_json_error_reporter, first) {""",
            modified_source,
        )
        self.assertEqual(modified_source.count(".report_error_aaaaa"), 1)
        self.assertEqual(
            modified_source.count("test_vim_qflist_json_error_reporter, aaaaa"), 1
        )

    def test_add_error_type_after_last_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="zzzzz", arguments=())
        )
        self.assertIn(
            """\
  EXPECT_EQ(qflist[0]["text"], "second");
}

TEST_F(test_vim_qflist_json_error_reporter, zzzzz) {
  padded_string input(u8"TODO");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report_error_zzzzz();
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 1 /* TODO */);
  EXPECT_EQ(qflist[0]["end_col"], 1 /* TODO */);
  EXPECT_EQ(qflist[0]["end_lnum"], 1 /* TODO */);
  EXPECT_EQ(qflist[0]["lnum"], 1 /* TODO */);
  EXPECT_EQ(qflist[0]["text"], "TODO");
}
}
}
""",
            modified_source,
        )
        self.assertEqual(modified_source.count(".report_error_zzzzz"), 1)
        self.assertEqual(
            modified_source.count("test_vim_qflist_json_error_reporter, zzzzz"), 1
        )

    def test_add_error_type_in_middle_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="mmmmm", arguments=())
        )
        self.assertIn(
            """\
  EXPECT_EQ(qflist[0]["text"], "first");
}

TEST_F(test_vim_qflist_json_error_reporter, mmmmm) {
  padded_string input(u8"TODO");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report_error_mmmmm();
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 1 /* TODO */);
  EXPECT_EQ(qflist[0]["end_col"], 1 /* TODO */);
  EXPECT_EQ(qflist[0]["end_lnum"], 1 /* TODO */);
  EXPECT_EQ(qflist[0]["lnum"], 1 /* TODO */);
  EXPECT_EQ(qflist[0]["text"], "TODO");
}

TEST_F(test_vim_qflist_json_error_reporter, second) {""",
            modified_source,
        )
        self.assertEqual(modified_source.count(".report_error_mmmmm"), 1)
        self.assertEqual(
            modified_source.count("test_vim_qflist_json_error_reporter, mmmmm"), 1
        )


class TestAddErrorCollectorFunction(unittest.TestCase):
    original_source = """\
struct error_collector : public error_reporter {
  void report_error_first(source_code_span where) override {
    this->errors.emplace_back(error_first, where);
  }

  void report_error_second(
      identifier left, identifier right, variable_kind) override {
    this->errors.emplace_back(error_second, left.span(), right.span());
  }

  enum error_kind {
    error_first,
    error_second,
  };
};
"""

    def test_add_error_type_before_first_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="aaaaa", arguments=())
        )
        self.assertIn(
            """\
struct error_collector : public error_reporter {
  void report_error_aaaaa() override {
    this->errors.emplace_back(error_aaaaa);
  }

  void report_error_first(source_code_span where) override {""",
            modified_source,
        )
        self.assertEqual(modified_source.count("report_error_aaaaa"), 1)

        self.assertIn(
            """\
  enum error_kind {
    error_aaaaa,
    error_first,""",
            modified_source,
        )
        self.assertEqual(modified_source.count(" error_aaaaa"), 1)

    def test_add_error_type_after_last_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="zzzzz", arguments=())
        )
        self.assertIn(
            """\
  void report_error_second(
      identifier left, identifier right, variable_kind) override {
    this->errors.emplace_back(error_second, left.span(), right.span());
  }

  void report_error_zzzzz() override {
    this->errors.emplace_back(error_zzzzz);
  }""",
            modified_source,
        )
        self.assertEqual(modified_source.count("report_error_zzzzz"), 1)

        self.assertIn(
            """\
    error_second,
    error_zzzzz,
  };""",
            modified_source,
        )
        self.assertEqual(modified_source.count(" error_zzzzz"), 1)

    def test_add_error_type_in_middle_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="mmmmm", arguments=())
        )
        self.assertIn(
            """\
    this->errors.emplace_back(error_first, where);
  }

  void report_error_mmmmm() override {
    this->errors.emplace_back(error_mmmmm);
  }

  void report_error_second(
      identifier left, identifier right, variable_kind) override {""",
            modified_source,
        )
        self.assertEqual(modified_source.count("report_error_mmmmm"), 1)

        self.assertIn(
            """\
    error_first,
    error_mmmmm,
    error_second,""",
            modified_source,
        )
        self.assertEqual(modified_source.count(" error_mmmmm"), 1)

    def test_single_location_argument(self) -> None:
        modified_source = modify_source(
            self.original_source,
            ErrorType(name="test", arguments=(("source_code_span", "where"),)),
        )
        self.assertIn(
            """\
  void report_error_test(source_code_span where) override {
    this->errors.emplace_back(error_test, where);
  }
""",
            modified_source,
        )


class TestAddErrorCollectorErrorPrintTo(unittest.TestCase):
    original_source = """\
void PrintTo(const error_collector::error &x, std::ostream *out) {
#define QLJS_CASE(k)       \\
  case error_collector::k: \\
    *out << #k;            \\
    break;
  switch (x.kind) {
    QLJS_CASE(error_first)
    QLJS_CASE(error_second)
  }
#undef QLJS_CASE
}
"""

    def test_add_error_type_before_first_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="aaaaa", arguments=())
        )
        self.assertIn(
            """\
  switch (x.kind) {
    QLJS_CASE(error_aaaaa)
    QLJS_CASE(error_first)""",
            modified_source,
        )
        self.assertEqual(modified_source.count("(error_aaaaa)"), 1)

    def test_add_error_type_after_last_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="zzzzz", arguments=())
        )
        self.assertIn(
            """\
    QLJS_CASE(error_second)
    QLJS_CASE(error_zzzzz)
  }""",
            modified_source,
        )
        self.assertEqual(modified_source.count("(error_zzzzz)"), 1)

    def test_add_error_type_in_middle_alphabetically(self) -> None:
        modified_source = modify_source(
            self.original_source, ErrorType(name="mmmmm", arguments=())
        )
        self.assertIn(
            """\
    QLJS_CASE(error_first)
    QLJS_CASE(error_mmmmm)
    QLJS_CASE(error_second)""",
            modified_source,
        )
        self.assertEqual(modified_source.count("(error_mmmmm)"), 1)


if __name__ == "__main__":
    unittest.main()
