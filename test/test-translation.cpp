// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/diag/diag-reporter.h>
#include <quick-lint-js/diag/diagnostic-formatter.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/diag/diagnostic.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/i18n/translation-table-test-generated.h>
#include <quick-lint-js/i18n/translation.h>
#include <quick-lint-js/port/char8.h>
#include <string>
#include <vector>

using ::testing::ElementsAreArray;

namespace quick_lint_js {
namespace {
class Basic_Text_Diag_Reporter;
class Basic_Text_Diag_Formatter;

class Basic_Text_Diag_Formatter
    : public Diagnostic_Formatter<Basic_Text_Diag_Formatter> {
 public:
  explicit Basic_Text_Diag_Formatter(Basic_Text_Diag_Reporter *reporter);

  void write_before_message([[maybe_unused]] std::string_view code,
                            Diagnostic_Severity, const Source_Code_Span &) {}

  void write_message_part([[maybe_unused]] std::string_view code,
                          Diagnostic_Severity, String8_View part) {
    this->current_message_.append(part);
  }

  void write_after_message(std::string_view code, Diagnostic_Severity,
                           const Source_Code_Span &);

 private:
  Basic_Text_Diag_Reporter *reporter_;
  String8 current_message_;
};

class Basic_Text_Diag_Reporter final : public Diag_Reporter {
 public:
  explicit Basic_Text_Diag_Reporter(Translator t) : translator_(t) {}

  std::vector<String8> messages() { return this->messages_; }

  void report(const Diag_List &diags) override {
    diags.for_each([&](Diag_Type type, void *diag) -> void {
      Basic_Text_Diag_Formatter formatter(this);
      formatter.format(get_diagnostic_info(type), diag);
    });
  }

 private:
  std::vector<String8> messages_;
  Translator translator_;

  friend Basic_Text_Diag_Formatter;
};

Basic_Text_Diag_Formatter::Basic_Text_Diag_Formatter(
    Basic_Text_Diag_Reporter *reporter)
    : Diagnostic_Formatter<Basic_Text_Diag_Formatter>(reporter->translator_),
      reporter_(reporter) {}

void Basic_Text_Diag_Formatter::write_after_message(
    [[maybe_unused]] std::string_view code, Diagnostic_Severity,
    const Source_Code_Span &) {
  this->reporter_->messages_.emplace_back(std::move(this->current_message_));
}

class Test_Translation : public ::testing::Test {
 public:
  void TearDown() override { initialize_translations_from_locale("C"); }

 protected:
  Source_Code_Span dummy_span() {
    static const Char8 hello[] = u8"hello";
    return Source_Code_Span(&hello[0], &hello[5]);
  }

  Monotonic_Allocator memory_ = Monotonic_Allocator("Test_Translation");
};

TEST_F(Test_Translation, c_language_does_not_translate_diagnostics) {
  Translator t;
  t.use_messages_from_locale("C");
  Diag_List diags(&this->memory_);
  diags.add(Diag_Unexpected_Hash_Character{this->dummy_span()});
  Basic_Text_Diag_Reporter reporter(t);
  reporter.report(diags);
  EXPECT_THAT(reporter.messages(), ElementsAreArray({
                                       u8"unexpected '#'",
                                   }));
}

TEST_F(Test_Translation, english_snarky_translates) {
  Translator t;
  t.use_messages_from_locale("en_US.utf8@snarky");
  Diag_List diags(&this->memory_);
  diags.add(Diag_Unexpected_Hash_Character{this->dummy_span()});
  Basic_Text_Diag_Reporter reporter(t);
  reporter.report(diags);
  EXPECT_THAT(reporter.messages(), ElementsAreArray({u8"#unexpected"}));
}

TEST_F(Test_Translation, full_translation_table_translated) {
  for (std::size_t locale_index = 0;
       locale_index < std::size(test_locale_names); ++locale_index) {
    const char *locale_name = test_locale_names[locale_index];
    SCOPED_TRACE(locale_name);
    Translator messages;
    EXPECT_TRUE(messages.use_messages_from_locale(locale_name));

    for (const Translated_String &test_case : test_translation_table) {
      ASSERT_TRUE(test_case.translatable.valid());
      EXPECT_EQ(messages.translate(test_case.translatable),
                String8_View(test_case.expected_per_locale[locale_index]));
    }
  }
}

TEST_F(Test_Translation, full_translation_table_untranslated) {
  Translator messages;
  messages.use_messages_from_source_code();
  for (const Translated_String &test_case : test_translation_table) {
    ASSERT_TRUE(test_case.translatable.valid());
    EXPECT_EQ(messages.translate(test_case.translatable),
              String8_View(test_case.expected_untranslated));
  }
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
