// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/diagnostic-formatter.h>
#include <quick-lint-js/diagnostic.h>
#include <quick-lint-js/error-reporter.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/translation.h>
#include <string>
#include <vector>

using ::testing::ElementsAre;

namespace quick_lint_js {
namespace {
class basic_text_error_reporter;
class basic_text_error_formatter;

class basic_text_error_formatter
    : public diagnostic_formatter<basic_text_error_formatter> {
 public:
  explicit basic_text_error_formatter(basic_text_error_reporter *reporter)
      : reporter_(reporter) {}

  void write_before_message([[maybe_unused]] std::string_view code,
                            diagnostic_severity, const source_code_span &) {}

  void write_message_part([[maybe_unused]] std::string_view code,
                          diagnostic_severity, string8_view part) {
    this->current_message_.append(part);
  }

  void write_after_message(std::string_view code, diagnostic_severity,
                           const source_code_span &);

 private:
  basic_text_error_reporter *reporter_;
  string8 current_message_;
};

class basic_text_error_reporter final : public new_style_error_reporter {
 public:
  explicit basic_text_error_reporter() = default;

  std::vector<string8> messages() { return this->messages_; }

  void report_impl(error_type type, void *error) override {
    basic_text_error_formatter formatter(this);
    formatter.format(all_diagnostic_infos[static_cast<std::ptrdiff_t>(type)],
                     error);
  }

 private:
  std::vector<string8> messages_;

  friend basic_text_error_formatter;
};

void basic_text_error_formatter::write_after_message(
    [[maybe_unused]] std::string_view code, diagnostic_severity,
    const source_code_span &) {
  this->reporter_->messages_.emplace_back(std::move(this->current_message_));
}

class test_translation : public ::testing::Test {
 public:
  void TearDown() override { initialize_translations_from_locale("C"); }

 protected:
  basic_text_error_reporter reporter;

  source_code_span dummy_span() {
    static const char8 hello[] = u8"hello";
    return source_code_span(&hello[0], &hello[5]);
  }
};

TEST_F(test_translation, c_language_does_not_translate_diagnostics) {
  initialize_translations_from_locale("C");
  this->reporter.report(error_unexpected_hash_character{this->dummy_span()});
  EXPECT_THAT(this->reporter.messages(), ElementsAre(u8"unexpected '#'"));
}

TEST_F(test_translation, english_loud_language_upper_cases_base) {
  initialize_translations_from_locale("en.utf8@loud");
  this->reporter.report(error_unexpected_hash_character{this->dummy_span()});
  EXPECT_THAT(this->reporter.messages(), ElementsAre(u8"UNEXPECTED '#'"));
}
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew "strager" Glazar
//
// This file is part of quick-lint-js.
//
// quick-lint-js is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// quick-lint-js is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with quick-lint-js.  If not, see <https://www.gnu.org/licenses/>.
