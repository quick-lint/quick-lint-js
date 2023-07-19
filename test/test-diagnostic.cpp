// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <gtest/gtest.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/diag/diagnostic.h>
#include <quick-lint-js/port/char8.h>
#include <string_view>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
template <class Error>
inline const Diagnostic_Info& diagnostic_info_for_error =
    get_diagnostic_info(Diag_Type_From_Type<Error>);

TEST(Test_Diagnostic, diagnostic_info) {
  Translator source_code_translator;
  source_code_translator.use_messages_from_source_code();

  {
    const Diagnostic_Info& info = diagnostic_info_for_error<
        Diag_Expected_Parentheses_Around_If_Condition>;
    EXPECT_EQ(info.code, 17);
    EXPECT_EQ(info.severity, Diagnostic_Severity::error);
    EXPECT_EQ(source_code_translator.translate(info.message_formats[0]),
              u8"if statement needs parentheses around condition"_sv);
    EXPECT_EQ(
        info.message_args[0][0].offset(),
        offsetof(Diag_Expected_Parentheses_Around_If_Condition, condition));
    EXPECT_EQ(info.message_args[0][0].type,
              Diagnostic_Arg_Type::Source_Code_Span);
    EXPECT_FALSE(info.message_formats[1].valid());
  }

  {
    const Diagnostic_Info& info = diagnostic_info_for_error<
        Diag_Expected_Parenthesis_Around_If_Condition>;
    EXPECT_EQ(info.code, 18);
    EXPECT_EQ(info.severity, Diagnostic_Severity::error);
    EXPECT_EQ(source_code_translator.translate(info.message_formats[0]),
              u8"if statement is missing '{1}' around condition"_sv);
    EXPECT_EQ(info.message_args[0][0].offset(),
              offsetof(Diag_Expected_Parenthesis_Around_If_Condition, where));
    EXPECT_EQ(info.message_args[0][0].type,
              Diagnostic_Arg_Type::Source_Code_Span);
    EXPECT_EQ(info.message_args[0][1].offset(),
              offsetof(Diag_Expected_Parenthesis_Around_If_Condition, token));
    EXPECT_EQ(info.message_args[0][1].type, Diagnostic_Arg_Type::Char8);
    EXPECT_FALSE(info.message_formats[1].valid());
  }

  {
    const Diagnostic_Info& info = diagnostic_info_for_error<
        Diag_Function_Call_Before_Declaration_In_Block_Scope>;
    EXPECT_EQ(info.code, 77);
    EXPECT_EQ(info.severity, Diagnostic_Severity::warning);
    EXPECT_EQ(source_code_translator.translate(info.message_formats[0]),
              u8"function called before declaration in block scope: {0}"_sv);
    EXPECT_EQ(
        info.message_args[0][0].offset(),
        offsetof(Diag_Function_Call_Before_Declaration_In_Block_Scope, use));
    EXPECT_EQ(info.message_args[0][0].type,
              Diagnostic_Arg_Type::Source_Code_Span);
    EXPECT_EQ(source_code_translator.translate(info.message_formats[1]),
              u8"function declared here"_sv);
    EXPECT_EQ(info.message_args[1][0].offset(),
              offsetof(Diag_Function_Call_Before_Declaration_In_Block_Scope,
                       declaration));
    EXPECT_EQ(info.message_args[1][0].type,
              Diagnostic_Arg_Type::Source_Code_Span);
  }

  {
    const Diagnostic_Info& info =
        diagnostic_info_for_error<Diag_Class_Statement_Not_Allowed_In_Body>;
    EXPECT_EQ(info.code, 149);
    EXPECT_EQ(info.severity, Diagnostic_Severity::error);
    EXPECT_EQ(source_code_translator.translate(info.message_formats[0]),
              u8"missing body for {1:headlinese}"_sv);
    EXPECT_EQ(
        info.message_args[0][0].offset(),
        offsetof(Diag_Class_Statement_Not_Allowed_In_Body, expected_body));
    EXPECT_EQ(info.message_args[0][0].type,
              Diagnostic_Arg_Type::Source_Code_Span);
    EXPECT_EQ(
        info.message_args[0][1].offset(),
        offsetof(Diag_Class_Statement_Not_Allowed_In_Body, kind_of_statement));
    EXPECT_EQ(info.message_args[0][1].type,
              Diagnostic_Arg_Type::statement_kind);
    EXPECT_EQ(
        source_code_translator.translate(info.message_formats[1]),
        u8"a class statement is not allowed as the body of {1:singular}"_sv);
    EXPECT_EQ(
        info.message_args[1][0].offset(),
        offsetof(Diag_Class_Statement_Not_Allowed_In_Body, class_keyword));
    EXPECT_EQ(info.message_args[1][0].type,
              Diagnostic_Arg_Type::Source_Code_Span);
    EXPECT_EQ(
        info.message_args[1][1].offset(),
        offsetof(Diag_Class_Statement_Not_Allowed_In_Body, kind_of_statement));
    EXPECT_EQ(info.message_args[1][1].type,
              Diagnostic_Arg_Type::statement_kind);
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
