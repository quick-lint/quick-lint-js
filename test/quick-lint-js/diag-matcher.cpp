// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <gmock/gmock.h>
#include <optional>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/diag/diagnostic.h>
#include <quick-lint-js/fe/lex.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/port/unreachable.h>
#include <vector>

namespace quick_lint_js {
Source_Code_Span Diag_Matcher_Arg::get_span(const void *error_object) const {
  const void *member_data =
      reinterpret_cast<const char *>(error_object) + this->member_offset;
  switch (this->member_type) {
  case Diagnostic_Arg_Type::source_code_span:
    return *static_cast<const Source_Code_Span *>(member_data);

  case Diagnostic_Arg_Type::char8:
  case Diagnostic_Arg_Type::enum_kind:
  case Diagnostic_Arg_Type::invalid:
  case Diagnostic_Arg_Type::statement_kind:
  case Diagnostic_Arg_Type::string8_view:
  case Diagnostic_Arg_Type::variable_kind:
    QLJS_ASSERT(false && "invalid arg type");
    break;
  }
  QLJS_UNREACHABLE();
}

Char8 Diag_Matcher_Arg::get_char8(const void *error_object) const {
  QLJS_ASSERT(this->member_type == Diagnostic_Arg_Type::char8);
  const void *member_data =
      reinterpret_cast<const char *>(error_object) + this->member_offset;
  return *static_cast<const Char8 *>(member_data);
}

Enum_Kind Diag_Matcher_Arg::get_enum_kind(const void *error_object) const {
  QLJS_ASSERT(this->member_type == Diagnostic_Arg_Type::enum_kind);
  const void *member_data =
      reinterpret_cast<const char *>(error_object) + this->member_offset;
  return *static_cast<const Enum_Kind *>(member_data);
}

String8_View Diag_Matcher_Arg::get_string8_view(
    const void *error_object) const {
  QLJS_ASSERT(this->member_type == Diagnostic_Arg_Type::string8_view);
  const void *member_data =
      reinterpret_cast<const char *>(error_object) + this->member_offset;
  return *static_cast<const String8_View *>(member_data);
}

Statement_Kind Diag_Matcher_Arg::get_statement_kind(
    const void *error_object) const {
  QLJS_ASSERT(this->member_type == Diagnostic_Arg_Type::statement_kind);
  const void *member_data =
      reinterpret_cast<const char *>(error_object) + this->member_offset;
  return *static_cast<const Statement_Kind *>(member_data);
}

Variable_Kind Diag_Matcher_Arg::get_variable_kind(
    const void *error_object) const {
  QLJS_ASSERT(this->member_type == Diagnostic_Arg_Type::variable_kind);
  const void *member_data =
      reinterpret_cast<const char *>(error_object) + this->member_offset;
  return *static_cast<const Variable_Kind *>(member_data);
}

template <class State, class Field>
class Diag_Fields_Matcher_Impl_Base
    : public testing::MatcherInterface<const Any_Diag_Pointer &> {
 public:
  explicit Diag_Fields_Matcher_Impl_Base(State s) : state_(std::move(s)) {}

  void DescribeTo(std::ostream *out) const final {
    *out << this->state_.type << "{";

    bool is_first_field = true;
    for (const Field &f : this->state_.fields) {
      if (!is_first_field) {
        *out << ", ";
      }
      this->describe_field(f, out);
      is_first_field = false;
    }

    *out << "}";
  }

  void DescribeNegationTo(std::ostream *out) const final {
    *out << "not ";
    this->DescribeTo(out);
  }

  bool MatchAndExplain(const Any_Diag_Pointer &error,
                       testing::MatchResultListener *listener) const final {
    bool type_matches = error.type == this->state_.type;
    if (!type_matches) {
      *listener << "whose type (" << error.type << ") isn't "
                << this->state_.type;
      return false;
    }

    bool result = true;
    bool is_first_field = true;
    for (const Field &f : this->state_.fields) {
      if (!is_first_field) {
        *listener << " and ";
      }
      bool matches = this->field_matches(error, f, listener);
      result = result && matches;
      is_first_field = false;
    }
    return result;
  }

 protected:
  virtual void describe_field(const Field &, std::ostream *out) const = 0;
  virtual bool field_matches(const Any_Diag_Pointer &error, const Field &f,
                             testing::MatchResultListener *listener) const = 0;

  State state_;
};

Diag_Matcher_2::Diag_Matcher_2(Padded_String_View input, Diag_Type type,
                               std::vector<Field> fields)
    : state_{type, input, std::move(fields)} {}

class Diag_Matcher_2::Impl final
    : public Diag_Fields_Matcher_Impl_Base<Diag_Matcher_2::State,
                                           Diag_Matcher_2::Field> {
 public:
  using Base = Diag_Fields_Matcher_Impl_Base<Diag_Matcher_2::State,
                                             Diag_Matcher_2::Field>;

  using Base::Diag_Fields_Matcher_Impl_Base;

 protected:
  void describe_field(const Field &f, std::ostream *out) const override {
    *out << "." << f.arg.member_name << " = ";
    switch (f.arg.member_type) {
    case Diagnostic_Arg_Type::source_code_span:
      *out << f.begin_offset << ".." << f.end_offset;
      break;

    case Diagnostic_Arg_Type::char8:
      *out << "'";
      // TODO(strager): Escape characters like ' and \ and (null) and (tab).
      *out << static_cast<char>(f.character);
      *out << "'";
      break;

    case Diagnostic_Arg_Type::string8_view:
      *out << "\"";
      // TODO(strager): Escape characters like ' and \ and (null) and (tab).
      *out << out_string8(f.string);
      *out << "\"";
      break;

    case Diagnostic_Arg_Type::enum_kind:
      *out << "Enum_Kind::";
      *out << f.enum_kind;
      break;

    case Diagnostic_Arg_Type::statement_kind:
      *out << "Statement_Kind::";
      *out << f.statement_kind;
      break;

    case Diagnostic_Arg_Type::variable_kind:
      *out << "Variable_Kind::";
      *out << f.variable_kind;
      break;

    case Diagnostic_Arg_Type::invalid:
      QLJS_UNREACHABLE();
      break;
    }
  }

  bool field_matches(const Any_Diag_Pointer &error, const Field &f,
                     testing::MatchResultListener *listener) const override {
    switch (f.arg.member_type) {
    case Diagnostic_Arg_Type::source_code_span: {
      Source_Code_Span span = f.arg.get_span(error.data);
      auto span_begin_offset = narrow_cast<CLI_Source_Position::Offset_Type>(
          span.begin() - this->state_.input.data());
      auto span_end_offset = narrow_cast<CLI_Source_Position::Offset_Type>(
          span.end() - this->state_.input.data());

      bool span_matches = span_begin_offset == f.begin_offset &&
                          span_end_offset == f.end_offset;
      *listener << "whose ." << f.arg.member_name << " (" << span_begin_offset
                << "-" << span_end_offset << ") "
                << (span_matches ? "equals" : "doesn't equal") << " "
                << f.begin_offset << "-" << f.end_offset;
      return span_matches;
    }

    case Diagnostic_Arg_Type::char8: {
      Char8 character = f.arg.get_char8(error.data);
      bool character_matches = character == f.character;
      *listener << "whose ." << f.arg.member_name << " ('"
                << static_cast<char>(character) << "') "
                << (character_matches ? "equals" : "doesn't equal") << " '"
                << static_cast<char>(f.character) << "'";
      return character_matches;
    }

    case Diagnostic_Arg_Type::enum_kind: {
      Enum_Kind enum_kind = f.arg.get_enum_kind(error.data);
      bool matches = enum_kind == f.enum_kind;
      *listener << "whose ." << f.arg.member_name << " (" << enum_kind << ") "
                << (matches ? "equals" : "doesn't equal") << " " << f.enum_kind;
      return matches;
    }

    case Diagnostic_Arg_Type::string8_view: {
      String8_View string = f.arg.get_string8_view(error.data);
      bool character_matches = string == f.string;
      *listener << "whose ." << f.arg.member_name << " (\""
                << to_string_view(string) << "\") "
                << (character_matches ? "equals" : "doesn't equal") << " \""
                << to_string_view(f.string) << "\"";
      return character_matches;
    }

    case Diagnostic_Arg_Type::statement_kind: {
      Statement_Kind statement_kind = f.arg.get_statement_kind(error.data);
      bool character_matches = statement_kind == f.statement_kind;
      *listener << "whose ." << f.arg.member_name << " (" << statement_kind
                << ") " << (character_matches ? "equals" : "doesn't equal")
                << " " << f.statement_kind;
      return character_matches;
    }

    case Diagnostic_Arg_Type::variable_kind: {
      Variable_Kind variable_kind = f.arg.get_variable_kind(error.data);
      bool character_matches = variable_kind == f.variable_kind;
      *listener << "whose ." << f.arg.member_name << " (" << variable_kind
                << ") " << (character_matches ? "equals" : "doesn't equal")
                << " " << f.variable_kind;
      return character_matches;
    }

    default:
      QLJS_ASSERT(false);
      return false;
    }
  }
};

Diag_Matcher_2::operator testing::Matcher<const Any_Diag_Pointer &>() const {
  return testing::Matcher<const Any_Diag_Pointer &>(new Impl(this->state_));
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
