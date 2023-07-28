// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/reflection/cxx-parser.h>
#include <string_view>
#include <vector>

using ::testing::ElementsAreArray;

namespace quick_lint_js {
namespace {
class Test_CXX_Lexer : private CLI_Locator, public CXX_Lexer {
 public:
  explicit Test_CXX_Lexer(Padded_String_View code)
      : CLI_Locator(code), CXX_Lexer(code, __FILE__ "(test)", this) {}
};

CXX_Diagnostic_Variable var(String8_View type) {
  return CXX_Diagnostic_Variable{
      .type = type,
      .name = u8""_sv,
  };
}

TEST(Test_CXX_Parser, lex_plain_string_literal) {
  Padded_String code(u8R"("hello world")"_sv);
  Test_CXX_Lexer l(&code);
  ASSERT_EQ(l.peek().type, CXX_Token_Type::string_literal);
  EXPECT_EQ(l.peek().decoded_string, u8"hello world"_sv);
}

TEST(Test_CXX_Parser, string_literal_decodes_escapes) {
  Padded_String code(u8R"("backslash=\\ newline=\n dquote=\" squote=\'")"_sv);
  Test_CXX_Lexer l(&code);
  ASSERT_EQ(l.peek().type, CXX_Token_Type::string_literal);
  EXPECT_EQ(l.peek().decoded_string,
            u8"backslash=\\ newline=\n dquote=\" squote='"_sv);
}

TEST(Test_CXX_Parser, adjacent_string_literals_concatenate) {
  {
    Padded_String code(u8R"("hello " "world")"_sv);
    Test_CXX_Lexer l(&code);
    ASSERT_EQ(l.peek().type, CXX_Token_Type::string_literal);
    EXPECT_EQ(l.peek().decoded_string, u8"hello world"_sv);
  }

  {
    Padded_String code(
        u8R"("hello "
"world")"_sv);
    Test_CXX_Lexer l(&code);
    ASSERT_EQ(l.peek().type, CXX_Token_Type::string_literal);
    EXPECT_EQ(l.peek().decoded_string, u8"hello world"_sv);
  }

  {
    Padded_String code(
        u8R"("hello "
// comment
"world")"_sv);
    Test_CXX_Lexer l(&code);
    ASSERT_EQ(l.peek().type, CXX_Token_Type::string_literal);
    EXPECT_EQ(l.peek().decoded_string, u8"hello world"_sv);
  }
}

TEST(Test_CXX_Parser, layout_offsets) {
  EXPECT_THAT(layout_offsets(Span<const CXX_Diagnostic_Variable>()),
              ::testing::IsEmpty());

  for (String8_View type : {
           u8"Char8"_sv,
           u8"Enum_Kind"_sv,
           u8"Source_Code_Span"_sv,
           u8"Statement_Kind"_sv,
           u8"String8_View"_sv,
           u8"Variable_Kind"_sv,
       }) {
    SCOPED_TRACE(out_string8(type));
    EXPECT_THAT(
        layout_offsets(Span<const CXX_Diagnostic_Variable>({var(type)})),
        ElementsAreArray({0}))
        << "first member variable always has offset 0";
  }

  {
    struct S {
      Source_Code_Span a;
      Source_Code_Span b;
    };
    EXPECT_THAT(layout_offsets(Span<const CXX_Diagnostic_Variable>({
                    var(u8"Source_Code_Span"_sv),
                    var(u8"Source_Code_Span"_sv),
                })),
                ElementsAreArray({
                    offsetof(S, a),
                    offsetof(S, b),
                }));
  }

  {
    struct S {
      Source_Code_Span a;
      Char8 b;
    };
    EXPECT_THAT(layout_offsets(Span<const CXX_Diagnostic_Variable>({
                    var(u8"Source_Code_Span"_sv),
                    var(u8"Char8"_sv),
                })),
                ElementsAreArray({
                    offsetof(S, a),
                    offsetof(S, b),
                }));
  }

  {
    struct S {
      Char8 a;
      Source_Code_Span b;
    };
    EXPECT_THAT(layout_offsets(Span<const CXX_Diagnostic_Variable>({
                    var(u8"Char8"_sv),
                    var(u8"Source_Code_Span"_sv),
                })),
                ElementsAreArray({
                    offsetof(S, a),
                    offsetof(S, b),
                }))
        << "padding should be inserted between Char8 and Source_Code_Span";
  }

  {
    struct S {
      Char8 a;
      Char8 b;
    };
    EXPECT_THAT(layout_offsets(Span<const CXX_Diagnostic_Variable>({
                    var(u8"Char8"_sv),
                    var(u8"Char8"_sv),
                })),
                ElementsAreArray({
                    offsetof(S, a),
                    offsetof(S, b),
                }))
        << "padding should be inserted between Char8 and Source_Code_Span";
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
