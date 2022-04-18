// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/diagnostic.h>
#include <quick-lint-js/error.h>
#include <string_view>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
template <class Error>
inline const diagnostic_info& diagnostic_info_for_error =
    get_diagnostic_info(error_type_from_type<Error>);

TEST(test_diagnostic, diagnostic_info) {
  translatable_messages source_code_messages;
  source_code_messages.use_messages_from_source_code();

  {
    const diagnostic_info& info = diagnostic_info_for_error<
        error_expected_parentheses_around_if_condition>;
    EXPECT_EQ(info.code, "E0017"sv);
    EXPECT_EQ(info.severity, diagnostic_severity::error);
    EXPECT_STREQ(source_code_messages.translate(info.messages[0].format),
                 "if statement needs parentheses around condition");
    EXPECT_EQ(
        info.messages[0].args[0].offset,
        offsetof(error_expected_parentheses_around_if_condition, condition));
    EXPECT_EQ(info.messages[0].args[0].type,
              diagnostic_arg_type::source_code_span);
    EXPECT_FALSE(info.messages[1].format.valid());
  }

  {
    const diagnostic_info& info = diagnostic_info_for_error<
        error_expected_parenthesis_around_if_condition>;
    EXPECT_EQ(info.code, "E0018"sv);
    EXPECT_EQ(info.severity, diagnostic_severity::error);
    EXPECT_STREQ(source_code_messages.translate(info.messages[0].format),
                 "if statement is missing '{1}' around condition");
    EXPECT_EQ(info.messages[0].args[0].offset,
              offsetof(error_expected_parenthesis_around_if_condition, where));
    EXPECT_EQ(info.messages[0].args[0].type,
              diagnostic_arg_type::source_code_span);
    EXPECT_EQ(info.messages[0].args[1].offset,
              offsetof(error_expected_parenthesis_around_if_condition, token));
    EXPECT_EQ(info.messages[0].args[1].type, diagnostic_arg_type::char8);
    EXPECT_FALSE(info.messages[1].format.valid());
  }

  {
    const diagnostic_info& info = diagnostic_info_for_error<
        error_function_call_before_declaration_in_block_scope>;
    EXPECT_EQ(info.code, "E0077"sv);
    EXPECT_EQ(info.severity, diagnostic_severity::warning);
    EXPECT_STREQ(source_code_messages.translate(info.messages[0].format),
                 "function called before declaration in block scope: {0}");
    EXPECT_EQ(
        info.messages[0].args[0].offset,
        offsetof(error_function_call_before_declaration_in_block_scope, use));
    EXPECT_EQ(info.messages[0].args[0].type, diagnostic_arg_type::identifier);
    EXPECT_STREQ(source_code_messages.translate(info.messages[1].format),
                 "function declared here");
    EXPECT_EQ(info.messages[1].args[0].offset,
              offsetof(error_function_call_before_declaration_in_block_scope,
                       declaration));
    EXPECT_EQ(info.messages[1].args[0].type, diagnostic_arg_type::identifier);
  }

  {
    const diagnostic_info& info =
        diagnostic_info_for_error<error_class_statement_not_allowed_in_body>;
    EXPECT_EQ(info.code, "E0149"sv);
    EXPECT_EQ(info.severity, diagnostic_severity::error);
    EXPECT_STREQ(source_code_messages.translate(info.messages[0].format),
                 "missing body for {1:headlinese}");
    EXPECT_EQ(
        info.messages[0].args[0].offset,
        offsetof(error_class_statement_not_allowed_in_body, expected_body));
    EXPECT_EQ(info.messages[0].args[0].type,
              diagnostic_arg_type::source_code_span);
    EXPECT_EQ(
        info.messages[0].args[1].offset,
        offsetof(error_class_statement_not_allowed_in_body, kind_of_statement));
    EXPECT_EQ(info.messages[0].args[1].type,
              diagnostic_arg_type::statement_kind);
    EXPECT_STREQ(
        source_code_messages.translate(info.messages[1].format),
        "a class statement is not allowed as the body of {1:singular}");
    EXPECT_EQ(
        info.messages[1].args[0].offset,
        offsetof(error_class_statement_not_allowed_in_body, class_keyword));
    EXPECT_EQ(info.messages[1].args[0].type,
              diagnostic_arg_type::source_code_span);
    EXPECT_EQ(
        info.messages[1].args[1].offset,
        offsetof(error_class_statement_not_allowed_in_body, kind_of_statement));
    EXPECT_EQ(info.messages[1].args[1].type,
              diagnostic_arg_type::statement_kind);
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
