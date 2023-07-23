// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdio>
#include <cstdlib>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/hash-map.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/diag/diagnostic.h>
#include <quick-lint-js/diagnostic-assertion.h>
#include <quick-lint-js/gtest.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/cpp.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <type_traits>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
// To reduce build bloat, a Diagnostic_Assertion should not have code generated
// to destroy it.
static_assert(std::is_trivially_destructible_v<Diagnostic_Assertion>);

namespace {
#define QLJS_DIAG_TYPE_NAME(name) \
  {QLJS_CPP_QUOTE_U8_SV(name), ::quick_lint_js::Diag_Type::name},
Hash_Map<String8_View, Diag_Type> diag_type_name_to_diag_type =
    Hash_Map<String8_View, Diag_Type>{QLJS_X_DIAG_TYPE_NAMES};
#undef QLJS_DIAG_TYPE_NAME

bool is_diag_type_char(Char8 c) {
  return (u8'a' <= c && c <= u8'z') ||  //
         (u8'A' <= c && c <= u8'Z') ||  //
         (u8'0' <= c && c <= u8'9') ||  //
         c == u8'_';
}

std::optional<Enum_Kind> try_parse_enum_kind(String8_View);
std::optional<Statement_Kind> try_parse_statement_kind(String8_View);
}

Result<Diagnostic_Assertion, std::vector<std::string>>
Diagnostic_Assertion::parse(const Char8* specification) {
  struct Diagnostic_Assertion_Lexer {
    explicit Diagnostic_Assertion_Lexer(const Char8* specification)
        : p(specification) {}

    Padded_String_Size parse_leading_spaces() {
      Padded_String_Size leading_space_count = 0;
      for (; *p == u8' '; ++p) {
        leading_space_count += 1;
      }
      return leading_space_count;
    }

    Padded_String_Size parse_span_carets() {
      if (*p == u8'`') {
        ++p;
        return 0;
      } else {
        Padded_String_Size caret_count = 0;
        for (; *p == u8'^'; ++p) {
          caret_count += 1;
        }
        return caret_count;
      }
    }

    void skip_spaces() { (void)this->parse_leading_spaces(); }

    // Parse '.' followed by an identifier.
    //
    // Returns an empty String8_View if there was no '.'.
    String8_View try_parse_dot_identifier() {
      if (*p != u8'.') {
        return String8_View();
      }
      ++p;
      String8_View span = this->parse_identifier();
      if (span.empty()) {
        this->errors.push_back("expected member variable name after '.'");
      }
      return span;
    }

    String8_View parse_identifier() {
      const Char8* begin = p;
      for (; is_diag_type_char(*p); ++p) {
      }
      const Char8* end = p;
      return make_string_view(begin, end);
    }

    String8_View parse_to_closing_curly_at_end_of_line() {
      const Char8* begin = p;
      for (; *p != u8'\0'; ++p) {
      }
      if (p == begin || p[-1] != u8'}') {
        errors.push_back("expected closing '}'");
        return String8_View();
      }
      const Char8* end = p - 1;
      QLJS_ASSERT(*p == u8'\0');
      return make_string_view(begin, end);
    }

    const Char8* p;

    std::vector<std::string> errors;
  };
  Diagnostic_Assertion_Lexer lexer(specification);

  Diagnostic_Assertion out_assertion;

  out_assertion.members[0].span_begin_offset = lexer.parse_leading_spaces();
  out_assertion.members[0].span_end_offset =
      out_assertion.members[0].span_begin_offset + lexer.parse_span_carets();
  lexer.skip_spaces();

  String8_View diag_type_span = lexer.parse_identifier();
  String8_View diag_member_span = lexer.try_parse_dot_identifier();

  String8_View diag_member_2_span;
  if (*lexer.p == u8'\n') {
    ++lexer.p;
    out_assertion.members[1].span_begin_offset = lexer.parse_leading_spaces();
    out_assertion.members[1].span_end_offset =
        out_assertion.members[1].span_begin_offset + lexer.parse_span_carets();
    lexer.skip_spaces();
    diag_member_2_span = lexer.try_parse_dot_identifier();
  }

  String8_View diag_member_3_span;
  if (*lexer.p == u8'\n') {
    ++lexer.p;
    out_assertion.members[2].span_begin_offset = lexer.parse_leading_spaces();
    out_assertion.members[2].span_end_offset =
        out_assertion.members[2].span_begin_offset + lexer.parse_span_carets();
    lexer.skip_spaces();
    diag_member_3_span = lexer.try_parse_dot_identifier();
  }

  String8_View extra_member_span;
  String8_View extra_member_value_span;
  if (*lexer.p == u8'{' && !diag_member_span.empty()) {
    ++lexer.p;
    if (*lexer.p != u8'.') {
      goto done_unexpected;
    }
    ++lexer.p;
    extra_member_span = lexer.parse_identifier();
    if (extra_member_span.empty()) {
      lexer.errors.push_back("expected member variable name after '{.'");
    }

    if (*lexer.p != u8'=') {
      extra_member_span = String8_View();
      goto done_unexpected;
    }
    ++lexer.p;

    extra_member_value_span = lexer.parse_to_closing_curly_at_end_of_line();
  }

  if (*lexer.p != u8'\0') {
    if (*lexer.p == ' ') {
      lexer.errors.push_back("trailing whitespace is not allowed in _diag");
    } else {
    done_unexpected:
      lexer.errors.push_back(concat("unexpected '"sv,
                                    to_string_view(String8_View(lexer.p, 1)),
                                    "' in _diag"sv));
    }
  }

  auto diag_type_it = diag_type_name_to_diag_type.find(diag_type_span);
  if (diag_type_it == diag_type_name_to_diag_type.end()) {
    if (lexer.errors.empty()) {
      lexer.errors.push_back(concat("invalid diagnostic type: '"sv,
                                    to_string_view(diag_type_span), "'"sv));
    }
    return failed_result(std::move(lexer.errors));
  }

  out_assertion.type = diag_type_it->second;
  const Diagnostic_Info_Debug& diag_info =
      get_diagnostic_info_debug(out_assertion.type);
  bool member_is_required = diag_info.variable_count() > 1;
  if (member_is_required && diag_member_span.empty()) {
    std::string members;
    for (const Diagnostic_Info_Variable_Debug& var : diag_info.variables) {
      if (var.name != nullptr &&
          var.type == Diagnostic_Arg_Type::source_code_span) {
        if (!members.empty()) {
          members += " or "sv;
        }
        members += '.';
        members += var.name;
      }
    }
    lexer.errors.push_back(concat("member required for "sv,
                                  to_string_view(diag_type_span), "; try "sv,
                                  members));
  }

  std::size_t member_index = 0;
  {
    const Diagnostic_Info_Variable_Debug* member;
    if (diag_member_span.empty()) {
      // Default to the first Source_Code_Span member.
      member = diag_info.find_first(Diagnostic_Arg_Type::source_code_span);
    } else {
      member = diag_info.find(to_string_view(diag_member_span));
    }
    QLJS_ALWAYS_ASSERT(member != nullptr);

    out_assertion.members[member_index].name = member->name;
    out_assertion.members[member_index].offset = member->offset;
    out_assertion.members[member_index].type = member->type;
    member_index += 1;
  }

  if (!diag_member_2_span.empty()) {
    const Diagnostic_Info_Variable_Debug* member =
        diag_info.find(to_string_view(diag_member_2_span));
    QLJS_ALWAYS_ASSERT(member != nullptr);

    out_assertion.members[member_index].name = member->name;
    out_assertion.members[member_index].offset = member->offset;
    out_assertion.members[member_index].type = member->type;
    member_index += 1;
  }

  if (!diag_member_3_span.empty()) {
    const Diagnostic_Info_Variable_Debug* member =
        diag_info.find(to_string_view(diag_member_3_span));
    QLJS_ALWAYS_ASSERT(member != nullptr);

    out_assertion.members[member_index].name = member->name;
    out_assertion.members[member_index].offset = member->offset;
    out_assertion.members[member_index].type = member->type;
    member_index += 1;
  }

  if (!extra_member_span.empty()) {
    const Diagnostic_Info_Variable_Debug* extra_member =
        diag_info.find(to_string_view(extra_member_span));
    QLJS_ALWAYS_ASSERT(extra_member != nullptr);
    switch (extra_member->type) {
    case Diagnostic_Arg_Type::char8:
      if (extra_member_value_span.size() != 1) {
        lexer.errors.push_back(
            concat("member {."sv, to_string_view(extra_member_span),
                   "} is a Char8 but the given value is not one byte"sv));
        return failed_result(std::move(lexer.errors));
      }
      out_assertion.members[member_index].name = extra_member->name;
      out_assertion.members[member_index].offset = extra_member->offset;
      out_assertion.members[member_index].type = extra_member->type;
      out_assertion.members[member_index].character =
          extra_member_value_span[0];
      break;

    case Diagnostic_Arg_Type::enum_kind: {
      out_assertion.members[member_index].name = extra_member->name;
      out_assertion.members[member_index].offset = extra_member->offset;
      out_assertion.members[member_index].type = extra_member->type;
      std::optional<Enum_Kind> enum_kind =
          try_parse_enum_kind(extra_member_value_span);
      if (enum_kind.has_value()) {
        out_assertion.members[member_index].enum_kind = *enum_kind;
      } else {
        lexer.errors.push_back(concat("invalid Enum_Kind: "sv,
                                      to_string_view(extra_member_value_span)));
      }
      break;
    }

    case Diagnostic_Arg_Type::string8_view:
      out_assertion.members[member_index].name = extra_member->name;
      out_assertion.members[member_index].offset = extra_member->offset;
      out_assertion.members[member_index].type = extra_member->type;
      out_assertion.members[member_index].string = extra_member_value_span;
      break;

    case Diagnostic_Arg_Type::statement_kind: {
      out_assertion.members[member_index].name = extra_member->name;
      out_assertion.members[member_index].offset = extra_member->offset;
      out_assertion.members[member_index].type = extra_member->type;
      std::optional<Statement_Kind> statement_kind =
          try_parse_statement_kind(extra_member_value_span);
      if (statement_kind.has_value()) {
        out_assertion.members[member_index].statement_kind = *statement_kind;
      } else {
        lexer.errors.push_back(concat("invalid Statement_Kind: "sv,
                                      to_string_view(extra_member_value_span)));
      }
      break;
    }

    default:
      lexer.errors.push_back(concat("member {."sv,
                                    to_string_view(extra_member_span),
                                    "} has unsupported type"sv));
      break;
    }
  }

  if (!lexer.errors.empty()) {
    return failed_result(std::move(lexer.errors));
  }
  return out_assertion;
}

int Diagnostic_Assertion::member_count() const {
  int count = 0;
  for (const Diagnostic_Assertion::Member& member : this->members) {
    if (member.name != nullptr) {
      count += 1;
    }
  }
  return count;
}

Diagnostic_Assertion Diagnostic_Assertion::parse_or_exit(
    const Char8* specification) {
  Result<Diagnostic_Assertion, std::vector<std::string>> da =
      Diagnostic_Assertion::parse(specification);
  if (!da.ok()) {
    std::fprintf(stderr, "error while parsing \"%s\"_diag:\n",
                 reinterpret_cast<const char*>(specification));
    for (const std::string& s : da.error()) {
      std::fprintf(stderr, "%s\n", s.c_str());
    }
    std::exit(1);
  }
  return *da;
}

Diagnostic_Assertion Diagnostic_Assertion::adjusted_for_escaped_characters(
    String8_View code) const {
  static auto has_utf8_continuation = [](Char8 c) -> bool {
    return (static_cast<std::uint8_t>(c) & 0x80) != 0;
  };
  Padded_String_Size i;
  // Returns an adjustment.
  auto advance_character = [&code, &i]() -> int {
    Char8 c = code[narrow_cast<std::size_t>(i)];
    if (has_utf8_continuation(c)) {
      // Assumption: Code points in the range U+0080 through U+ffff are written
      // as "\u1234" (6 source characters). It could have been written literally
      // or as "\U00001234", but we don't handle those cases.
      //
      // Assumption: Code points in the range U+10000 through U+10ffff are
      // written as "\U00102345" (10 source characters). It could have been
      // written literally, but we don't handle that case.
      int character_byte_count = 1;
      ++i;
      while (i < narrow_cast<Padded_String_Size>(code.size()) &&
             has_utf8_continuation(code[narrow_cast<std::size_t>(i)])) {
        character_byte_count += 1;
        ++i;
      }
      int source_width = character_byte_count <= 3 ? 6 : 10;
      return character_byte_count - source_width;
    } else {
      ++i;
      static constexpr String8_View escaped_single_characters =
          u8"\b\n\t\"\\"_sv;
      if (escaped_single_characters.find(c) != String8_View::npos) {
        int source_width = 2;
        int character_byte_count = 1;
        return character_byte_count - source_width;
      }
      return 0;
    }
  };

  Diagnostic_Assertion result = *this;
  for (Member& member : result.members) {
    if (member.type != Diagnostic_Arg_Type::source_code_span) {
      continue;
    }
    for (i = 0; i < member.span_begin_offset;) {
      int adjustment = advance_character();
      member.span_begin_offset += adjustment;
      member.span_end_offset += adjustment;
    }
    for (; i < member.span_end_offset;) {
      int adjustment = advance_character();
      member.span_end_offset += adjustment;
    }
  }
  return result;
}

Diagnostic_Assertion operator""_diag(
    const Char8* specification,
    [[maybe_unused]] std::size_t specification_length) {
  return Diagnostic_Assertion::parse_or_exit(specification);
}

void assert_diagnostics(Padded_String_View code,
                        const std::vector<Diag_Collector::Diag>& diagnostics,
                        Span<const Diagnostic_Assertion> assertions,
                        Source_Location caller) {
  EXPECT_THAT_AT_CALLER(diagnostics, diagnostics_matcher(code, assertions));
}

void assert_diagnostics(Padded_String_View code,
                        const std::vector<Diag_Collector::Diag>& diagnostics,
                        std::initializer_list<Diagnostic_Assertion> assertions,
                        Source_Location caller) {
  assert_diagnostics(code, diagnostics,
                     Span<const Diagnostic_Assertion>(assertions), caller);
}

::testing::Matcher<const std::vector<Diag_Collector::Diag>&>
diagnostics_matcher(Padded_String_View code,
                    Span<const Diagnostic_Assertion> assertions) {
  std::vector<Diag_Matcher_2> error_matchers;
  for (const Diagnostic_Assertion& diag : assertions) {
    Diagnostic_Assertion adjusted_diag =
        diag.adjusted_for_escaped_characters(code.string_view());

    std::vector<Diag_Matcher_2::Field> fields;
    int member_count = adjusted_diag.member_count();
    for (int i = 0; i < member_count; ++i) {
      const Diagnostic_Assertion::Member& member =
          adjusted_diag.members[narrow_cast<std::size_t>(i)];
      Diag_Matcher_2::Field field;
      field.arg = Diag_Matcher_Arg{
          .member_name = member.name,
          .member_offset = member.offset,
          .member_type = member.type,
      };
      switch (member.type) {
      case Diagnostic_Arg_Type::source_code_span:
        field.begin_offset = narrow_cast<CLI_Source_Position::Offset_Type>(
            member.span_begin_offset);
        field.end_offset = narrow_cast<CLI_Source_Position::Offset_Type>(
            member.span_end_offset);
        break;
      case Diagnostic_Arg_Type::char8:
        field.character = member.character;
        break;
      case Diagnostic_Arg_Type::enum_kind:
        field.enum_kind = member.enum_kind;
        break;
      case Diagnostic_Arg_Type::string8_view:
        field.string = member.string;
        break;
      case Diagnostic_Arg_Type::statement_kind:
        field.statement_kind = member.statement_kind;
        break;
      default:
        QLJS_ASSERT(false);
        break;
      }
      fields.push_back(field);
    }

    error_matchers.push_back(
        Diag_Matcher_2(code, adjusted_diag.type, std::move(fields)));
  }
  if (error_matchers.size() <= 1) {
    // ElementsAreArray produces better diagnostics than
    // UnorderedElementsAreArray.
    return ::testing::ElementsAreArray(std::move(error_matchers));
  } else {
    return ::testing::UnorderedElementsAreArray(std::move(error_matchers));
  }
}

::testing::Matcher<const std::vector<Diag_Collector::Diag>&>
diagnostics_matcher(Padded_String_View code,
                    std::initializer_list<Diagnostic_Assertion> assertions) {
  return diagnostics_matcher(code,
                             Span<const Diagnostic_Assertion>(assertions));
}

namespace {
std::optional<Enum_Kind> try_parse_enum_kind(String8_View s) {
#define QLJS_CASE(kind)                                     \
  if (s == u8"Enum_Kind::"_sv QLJS_CPP_QUOTE_U8_SV(kind)) { \
    return Enum_Kind::kind;                                 \
  }
  QLJS_CASE(declare_const_enum)
  QLJS_CASE(const_enum)
  QLJS_CASE(declare_enum)
  QLJS_CASE(normal)
  return std::nullopt;
#undef QLJS_CASE
}

std::optional<Statement_Kind> try_parse_statement_kind(String8_View s) {
#define QLJS_CASE(kind)                                          \
  if (s == u8"Statement_Kind::"_sv QLJS_CPP_QUOTE_U8_SV(kind)) { \
    return Statement_Kind::kind;                                 \
  }
  QLJS_CASE(do_while_loop)
  QLJS_CASE(for_loop)
  QLJS_CASE(if_statement)
  QLJS_CASE(while_loop)
  QLJS_CASE(with_statement)
  QLJS_CASE(labelled_statement)
  return std::nullopt;
#undef QLJS_CASE
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
