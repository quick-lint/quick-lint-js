// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstddef>
#include <gmock/gmock.h>
#include <optional>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/diag/diagnostic.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/port/char8.h>
#include <vector>

namespace quick_lint_js {
// Metadata for a member of a diagnostic class.
struct Diag_Matcher_Arg {
  std::string_view member_name;
  std::size_t member_offset;
  Diagnostic_Arg_Type member_type;

  // Precondition: this->member_type == Diagnostic_Arg_Type::source_code_span
  Source_Code_Span get_span(const void *error_object) const;

  // Precondition: this->member_type == Diagnostic_Arg_Type::char8
  Char8 get_char8(const void *error_object) const;

  // Precondition: this->member_type == Diagnostic_Arg_Type::enum_kind
  Enum_Kind get_enum_kind(const void *error_object) const;

  // Precondition: this->member_type == Diagnostic_Arg_Type::string8_view
  String8_View get_string8_view(const void *error_object) const;

  // Precondition: this->member_type == Diagnostic_Arg_Type::statement_kind
  Statement_Kind get_statement_kind(const void *error_object) const;

  // Precondition: this->member_type == Diagnostic_Arg_Type::variable_kind
  Variable_Kind get_variable_kind(const void *error_object) const;
};

// Create a Diag_Matcher_Arg from a Diag_ struct type and the name of a member
// of that struct.
#define DIAG_MATCHER_ARG(type, member)                           \
  (::quick_lint_js::Diag_Matcher_Arg{                            \
      #member,                                                   \
      offsetof(type, member),                                    \
      ::quick_lint_js::get_diagnostic_message_arg_type<decltype( \
          type::member)>(),                                      \
  })

struct Any_Diag_Pointer {
  Diag_Type type;
  const void *data;
};

void PrintTo(const Any_Diag_Pointer &diag, std::ostream *out);

// A mix of ::testing::VariantWith, ::testing::Field, and Offsets_Matcher. These
// are combined into one matcher to significantly reduce compile times.
class Diag_Matcher_2 {
 public:
  struct Field {
    Diag_Matcher_Arg arg;

    // If this->arg.member_type == Diag_Matcher_Arg::source_code_span:
    CLI_Source_Position::Offset_Type begin_offset;
    CLI_Source_Position::Offset_Type end_offset;

    // If this->arg.member_type == Diag_Matcher_Arg::char8:
    Char8 character;

    // If this->arg.member_type == Diag_Matcher_Arg::enum_kind:
    Enum_Kind enum_kind;

    // If this->arg.member_type == Diag_Matcher_Arg::string8_view:
    String8_View string;

    // If this->arg.member_type == Diag_Matcher_Arg::statement_kind:
    Statement_Kind statement_kind;

    // If this->arg.member_type == Diag_Matcher_Arg::variable_kind:
    Variable_Kind variable_kind;
  };

  explicit Diag_Matcher_2(Padded_String_View input, Diag_Type type,
                          std::vector<Field>);

  Diag_Matcher_2(const Diag_Matcher_2 &) = default;
  Diag_Matcher_2(Diag_Matcher_2 &&) = default;
  Diag_Matcher_2 &operator=(const Diag_Matcher_2 &) = default;
  Diag_Matcher_2 &operator=(Diag_Matcher_2 &&) = default;

  /*implicit*/ operator testing::Matcher<const Any_Diag_Pointer &>() const;

  void DescribeTo(std::ostream *out) const {
    return ::testing::Matcher<const Any_Diag_Pointer &>(*this).DescribeTo(out);
  }

 private:
  class Impl;

  struct State {
    Diag_Type type;
    Padded_String_View input;
    std::vector<Field> fields;
  };

  State state_;
};
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
