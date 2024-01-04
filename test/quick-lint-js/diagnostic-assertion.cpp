// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdio>
#include <cstdlib>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/fixed-vector.h>
#include <quick-lint-js/container/hash-map.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diag/diag-list.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/diag/diagnostic.h>
#include <quick-lint-js/diagnostic-assertion.h>
#include <quick-lint-js/gtest.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/reflection/cxx-parser.h>
#include <quick-lint-js/util/cast.h>
#include <quick-lint-js/util/cpp.h>
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
std::optional<Variable_Kind> try_parse_variable_kind(String8_View);

struct Diagnostic_Info_Variable_Debug {
  String8_View name;
  Diagnostic_Arg_Type type;
  std::uint8_t offset;
};

struct Diagnostic_Info_Debug {
  Fixed_Vector<Diagnostic_Info_Variable_Debug, 4> variables;

  const Diagnostic_Info_Variable_Debug* find(String8_View name) const {
    for (const Diagnostic_Info_Variable_Debug& var : this->variables) {
      if (var.name == name) {
        return &var;
      }
    }
    return nullptr;
  }

  const Diagnostic_Info_Variable_Debug* find_first(
      Diagnostic_Arg_Type type) const {
    for (const Diagnostic_Info_Variable_Debug& var : this->variables) {
      if (var.type == type) {
        return &var;
      }
    }
    return nullptr;
  }
};

const Diagnostic_Info_Debug& get_diagnostic_info_debug(Diag_Type);
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

    std::optional<Padded_String_Size> parse_span_carets() {
      if (*p == u8'`') {
        ++p;
        return 0;
      } else if (*p == u8'^') {
        Padded_String_Size caret_count = 0;
        for (; *p == u8'^'; ++p) {
          caret_count += 1;
        }
        return caret_count;
      } else {
        return std::nullopt;
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

  // Names of diagnostic member variables.
  //
  // If an entry is empty, the member's name is inferred.
  Fixed_Vector<String8_View, 3> diag_members;

  String8_View diag_type_span;
  String8_View extra_member_span;
  String8_View extra_member_value_span;

  for (Fixed_Vector_Size i = 0; i < 3; ++i) {
    Padded_String_Size leading_spaces = lexer.parse_leading_spaces();
    std::optional<Padded_String_Size> span_carats = lexer.parse_span_carets();
    if (!span_carats.has_value()) {
      const Char8* old_p = lexer.p;

      diag_type_span = lexer.parse_identifier();

      String8_View member_variable = lexer.try_parse_dot_identifier();
      if (member_variable.empty()) {
        if (i != 0) {
          lexer.p = old_p;
          goto done_unexpected;
        }
      } else {
        if (i == 0) {
          lexer.errors.push_back(
              "member variable is only allowed if a span (^^^) is provided");
        } else {
          lexer.errors.push_back(concat("missing span (^^^) before ."sv,
                                        to_string_view(member_variable)));
        }
      }

      // If just the type was given with no span, then other member spans are
      // not allowed.
      break;
    }

    Member& member = out_assertion.members.emplace_back();
    member.span_begin_offset = leading_spaces;
    member.span_end_offset = member.span_begin_offset + *span_carats;

    lexer.skip_spaces();

    if (i == 0) {
      diag_type_span = lexer.parse_identifier();
    }
    diag_members.push_back(lexer.try_parse_dot_identifier());

    if (*lexer.p != u8'\n') {
      break;
    }
    ++lexer.p;
  }

  if (*lexer.p == u8'{' && (diag_members.empty() || !diag_members[0].empty())) {
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
  bool member_is_required = diag_info.variables.size() > 1;
  if (member_is_required &&
      (!diag_members.empty() && diag_members[0].empty())) {
    std::string members;
    for (const Diagnostic_Info_Variable_Debug& var : diag_info.variables) {
      if (var.type == Diagnostic_Arg_Type::source_code_span) {
        if (!members.empty()) {
          members += " or "sv;
        }
        members += '.';
        members += to_string_view(var.name);
      }
    }
    lexer.errors.push_back(concat("member required for "sv,
                                  to_string_view(diag_type_span), "; try "sv,
                                  members));
  }

  for (Fixed_Vector_Size i = 0; i < diag_members.size(); ++i) {
    const Diagnostic_Info_Variable_Debug* member;
    bool can_infer_member_variable = i == 0;
    if (can_infer_member_variable && diag_members[i].empty()) {
      // Default to the first Source_Code_Span member.
      member = diag_info.find_first(Diagnostic_Arg_Type::source_code_span);
    } else {
      member = diag_info.find(diag_members[i]);
    }
    QLJS_ALWAYS_ASSERT(member != nullptr);

    Member& out_member = out_assertion.members[i];
    out_member.name = member->name;
    out_member.offset = member->offset;
    out_member.type = member->type;
  }

  if (!extra_member_span.empty()) {
    const Diagnostic_Info_Variable_Debug* extra_member =
        diag_info.find(extra_member_span);
    QLJS_ALWAYS_ASSERT(extra_member != nullptr);

    Member& out_member = out_assertion.members.emplace_back();
    switch (extra_member->type) {
    case Diagnostic_Arg_Type::char8:
      if (extra_member_value_span.size() != 1) {
        lexer.errors.push_back(
            concat("member {."sv, to_string_view(extra_member_span),
                   "} is a Char8 but the given value is not one byte"sv));
        return failed_result(std::move(lexer.errors));
      }
      out_member.name = extra_member->name;
      out_member.offset = extra_member->offset;
      out_member.type = extra_member->type;
      out_member.character = extra_member_value_span[0];
      break;

    case Diagnostic_Arg_Type::enum_kind: {
      out_member.name = extra_member->name;
      out_member.offset = extra_member->offset;
      out_member.type = extra_member->type;
      std::optional<Enum_Kind> enum_kind =
          try_parse_enum_kind(extra_member_value_span);
      if (enum_kind.has_value()) {
        out_member.enum_kind = *enum_kind;
      } else {
        lexer.errors.push_back(concat("invalid Enum_Kind: "sv,
                                      to_string_view(extra_member_value_span)));
      }
      break;
    }

    case Diagnostic_Arg_Type::string8_view:
      out_member.name = extra_member->name;
      out_member.offset = extra_member->offset;
      out_member.type = extra_member->type;
      out_member.string = extra_member_value_span;
      break;

    case Diagnostic_Arg_Type::statement_kind: {
      out_member.name = extra_member->name;
      out_member.offset = extra_member->offset;
      out_member.type = extra_member->type;
      std::optional<Statement_Kind> statement_kind =
          try_parse_statement_kind(extra_member_value_span);
      if (statement_kind.has_value()) {
        out_member.statement_kind = *statement_kind;
      } else {
        lexer.errors.push_back(concat("invalid Statement_Kind: "sv,
                                      to_string_view(extra_member_value_span)));
      }
      break;
    }

    case Diagnostic_Arg_Type::variable_kind: {
      out_member.name = extra_member->name;
      out_member.offset = extra_member->offset;
      out_member.type = extra_member->type;
      std::optional<Variable_Kind> variable_kind =
          try_parse_variable_kind(extra_member_value_span);
      if (variable_kind.has_value()) {
        out_member.variable_kind = *variable_kind;
      } else {
        lexer.errors.push_back(concat("invalid Variable_Kind: "sv,
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

void assert_diagnostics(Padded_String_View code, const Diag_List& diagnostics,
                        Span<const Diagnostic_Assertion> assertions,
                        Source_Location caller) {
  EXPECT_THAT_AT_CALLER(diagnostics, diagnostics_matcher_2(code, assertions));
}

void assert_diagnostics(Padded_String_View code, const Diag_List& diagnostics,
                        std::initializer_list<Diagnostic_Assertion> assertions,
                        Source_Location caller) {
  assert_diagnostics(code, diagnostics,
                     Span<const Diagnostic_Assertion>(assertions), caller);
}

// TODO(#1154): Delete in favor of diagnostics_matcher_2.
::testing::Matcher<const std::vector<Diag_Collector::Diag>&>
diagnostics_matcher(Padded_String_View code,
                    Span<const Diagnostic_Assertion> assertions) {
  std::vector<Diag_Matcher_2> error_matchers;
  for (const Diagnostic_Assertion& diag : assertions) {
    Diagnostic_Assertion adjusted_diag =
        diag.adjusted_for_escaped_characters(code.string_view());

    std::vector<Diag_Matcher_2::Field> fields;
    for (const Diagnostic_Assertion::Member& member : adjusted_diag.members) {
      Diag_Matcher_2::Field field;
      field.arg = Diag_Matcher_Arg{
          .member_name = to_string_view(member.name),
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
      case Diagnostic_Arg_Type::variable_kind:
        field.variable_kind = member.variable_kind;
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
class Diag_List_Matcher_Impl
    : public testing::MatcherInterface<const Diag_List&> {
 public:
  explicit Diag_List_Matcher_Impl(std::vector<Diag_Matcher_2>&& error_matchers)
      : error_matchers_(std::move(error_matchers)) {}

  void DescribeTo([[maybe_unused]] std::ostream* out) const override {
    // FIXME(strager): Do we need to write anything here?
  }

  void DescribeNegationTo([[maybe_unused]] std::ostream* out) const override {
    // FIXME(strager): Do we need to write anything here?
  }

  bool MatchAndExplain(const Diag_List& diags,
                       testing::MatchResultListener* listener) const override {
    // TODO(strager): Write custom messages instead of delegating to Google
    // Test's built-ins.
    std::vector<Any_Diag_Pointer> diag_pointers;
    diags.for_each([&](Diag_Type type, const void* data) -> void {
      diag_pointers.push_back(Any_Diag_Pointer{.type = type, .data = data});
    });

    using Vector_Matcher =
        ::testing::Matcher<const std::vector<Any_Diag_Pointer>&>;
    Vector_Matcher vector_matcher =
        this->error_matchers_.size() <= 1
            ?
            // ElementsAreArray produces better diagnostics than
            // UnorderedElementsAreArray.
            Vector_Matcher(
                ::testing::ElementsAreArray(std::move(this->error_matchers_)))
            : Vector_Matcher(::testing::UnorderedElementsAreArray(
                  std::move(this->error_matchers_)));
    return vector_matcher.MatchAndExplain(diag_pointers, listener);
  }

 private:
  std::vector<Diag_Matcher_2> error_matchers_;
};
}

::testing::Matcher<const Diag_List&> diagnostics_matcher_2(
    Padded_String_View code, Span<const Diagnostic_Assertion> assertions) {
  std::vector<Diag_Matcher_2> error_matchers;
  for (const Diagnostic_Assertion& diag : assertions) {
    Diagnostic_Assertion adjusted_diag =
        diag.adjusted_for_escaped_characters(code.string_view());

    std::vector<Diag_Matcher_2::Field> fields;
    for (const Diagnostic_Assertion::Member& member : adjusted_diag.members) {
      Diag_Matcher_2::Field field;
      field.arg = Diag_Matcher_Arg{
          .member_name = to_string_view(member.name),
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
      case Diagnostic_Arg_Type::variable_kind:
        field.variable_kind = member.variable_kind;
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
  return ::testing::Matcher<const Diag_List&>(
      new Diag_List_Matcher_Impl(std::move(error_matchers)));
}

::testing::Matcher<const Diag_List&> diagnostics_matcher_2(
    Padded_String_View code,
    std::initializer_list<Diagnostic_Assertion> assertions) {
  return diagnostics_matcher_2(code,
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
  QLJS_CASE(class_implements_clause)
  QLJS_CASE(class_extends_clause)
  QLJS_CASE(interface_extends_clause)
  QLJS_CASE(typeof_type)
  return std::nullopt;
#undef QLJS_CASE
}

std::optional<Variable_Kind> try_parse_variable_kind(String8_View s) {
#define QLJS_CASE(kind)                                         \
  if (s == u8"Variable_Kind::"_sv QLJS_CPP_QUOTE_U8_SV(kind)) { \
    return Variable_Kind::kind;                                 \
  }
  QLJS_CASE(_arrow_parameter)
  QLJS_CASE(_catch)
  QLJS_CASE(_class)
  QLJS_CASE(_const)
  QLJS_CASE(_enum)
  QLJS_CASE(_function)
  QLJS_CASE(_function_parameter)
  QLJS_CASE(_function_type_parameter)
  QLJS_CASE(_generic_parameter)
  QLJS_CASE(_import)
  QLJS_CASE(_import_alias)
  QLJS_CASE(_import_type)
  QLJS_CASE(_index_signature_parameter)
  QLJS_CASE(_infer_type)
  QLJS_CASE(_interface)
  QLJS_CASE(_let)
  QLJS_CASE(_namespace)
  QLJS_CASE(_type_alias)
  QLJS_CASE(_var)
  return std::nullopt;
#undef QLJS_CASE
}

Diagnostic_Arg_Type parse_diagnostic_arg_type(String8_View code) {
#define QLJS_CASE(type_name, arg_type)             \
  do {                                             \
    if (code == QLJS_CPP_QUOTE_U8_SV(type_name)) { \
      return Diagnostic_Arg_Type::arg_type;        \
    }                                              \
  } while (false)

  QLJS_CASE(Char8, char8);
  QLJS_CASE(Enum_Kind, enum_kind);
  QLJS_CASE(Source_Code_Span, source_code_span);
  QLJS_CASE(Statement_Kind, statement_kind);
  QLJS_CASE(String8_View, string8_view);
  QLJS_CASE(Variable_Kind, variable_kind);

#undef QLJS_CASE

  QLJS_UNREACHABLE();
}

const Diagnostic_Info_Debug& get_diagnostic_info_debug(Diag_Type type) {
  QLJS_WARNING_PUSH
  QLJS_WARNING_IGNORE_GCC("-Wshadow=compatible-local")

  static Padded_String diagnostic_types_code = []() -> Padded_String {
    Result<Padded_String, Read_File_IO_Error> diagnostic_types_h =
        read_file(QLJS_DIAGNOSTIC_TYPES_H_FILE_PATH);
    if (!diagnostic_types_h.ok()) {
      std::fprintf(
          stderr,
          "fatal: failed to read diagnostics file for test reflection: %s\n",
          diagnostic_types_h.error_to_string().c_str());
      std::exit(1);
    }
    return *std::move(diagnostic_types_h);
  }();

  static std::vector<Diagnostic_Info_Debug> infos =
      []() -> std::vector<Diagnostic_Info_Debug> {
    CLI_Locator locator(&diagnostic_types_code);
    CXX_Diagnostic_Types_Parser parser(
        &diagnostic_types_code, QLJS_DIAGNOSTIC_TYPES_H_FILE_PATH, &locator);
    parser.parse_file();

    std::vector<Diagnostic_Info_Debug> infos;
    infos.reserve(narrow_cast<std::size_t>(parser.parsed_types.size()));
    for (CXX_Diagnostic_Type& diag_type : parser.parsed_types) {
      Diagnostic_Info_Debug& info = infos.emplace_back();
      Fixed_Vector<std::size_t, 4> variable_offsets = layout_offsets(
          Span<const CXX_Diagnostic_Variable>(diag_type.variables));
      for (Fixed_Vector_Size i = 0; i < diag_type.variables.size(); ++i) {
        CXX_Diagnostic_Variable& var = diag_type.variables[i];
        info.variables.push_back(Diagnostic_Info_Variable_Debug{
            .name = var.name,
            .type = parse_diagnostic_arg_type(var.type),
            .offset = narrow_cast<std::uint8_t>(variable_offsets[i]),
        });
      }
    }

    return infos;
  }();

  return infos.at(static_cast<std::size_t>(type));

  QLJS_WARNING_POP
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
