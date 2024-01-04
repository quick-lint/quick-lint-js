// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstddef>
#include <gmock/gmock.h>
#include <optional>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/diag/diagnostic.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/port/char8.h>
#include <vector>

#define DIAG_TYPE_FIELD(diag_type, member, matcher) \
  ::testing::VariantWith<diag_type>(                \
      ::testing::Field(#member, &diag_type::member, matcher))

// Equivalent to ::testing::VariantWith<type>(::testing::_), but compiles much
// more quickly.
#define DIAG_TYPE(type) \
  ::quick_lint_js::Diag_Matcher(::quick_lint_js::Diag_Type::type)

// Equivalent to the following, but compiles much more quickly:
//
//   DIAG_TYPE_FIELD(type, member_0,
//                    Offsets_Matcher(code, start_0, end_or_text_0))
//
// but compiles much more quickly.
#define DIAG_TYPE_OFFSETS(code, type, member_0, start_0, end_or_text_0) \
  ::quick_lint_js::Diag_Matcher(code, ::quick_lint_js::Diag_Type::type, \
                                ::quick_lint_js::Diag_Matcher::Field{   \
                                    DIAG_MATCHER_ARG(type, member_0),   \
                                    start_0,                            \
                                    end_or_text_0,                      \
                                })

// Equivalent to the following, but compiles much more quickly:
//
//   DIAG_TYPE_FIELD(type,
//                    member_0, Offsets_Matcher(code, start_0, end_or_text_0),
//                    member_1, Offsets_Matcher(code, start_1, end_or_text_1))
#define DIAG_TYPE_2_OFFSETS(code, type, member_0, start_0, end_or_text_0, \
                            member_1, start_1, end_or_text_1)             \
  ::quick_lint_js::Diag_Matcher(code, ::quick_lint_js::Diag_Type::type,   \
                                ::quick_lint_js::Diag_Matcher::Field{     \
                                    DIAG_MATCHER_ARG(type, member_0),     \
                                    start_0,                              \
                                    end_or_text_0,                        \
                                },                                        \
                                ::quick_lint_js::Diag_Matcher::Field{     \
                                    DIAG_MATCHER_ARG(type, member_1),     \
                                    start_1,                              \
                                    end_or_text_1,                        \
                                })

// Equivalent to the following, but compiles much more quickly:
//
//   DIAG_TYPE_FIELD(type, member_0, source_code_span_matcher(span_0))
//
// but compiles much more quickly.
#define DIAG_TYPE_SPAN(type, member_0, span_0)    \
  ::quick_lint_js::Diag_Spans_Matcher(            \
      ::quick_lint_js::Diag_Type::type,           \
      ::quick_lint_js::Diag_Spans_Matcher::Field{ \
          DIAG_MATCHER_ARG(type, member_0),       \
          span_0,                                 \
      })

// Equivalent to the following, but compiles much more quickly:
//
//   DIAG_TYPE_2_FIELD(type,
//                     member_0, source_code_span_matcher(span_0),
//                     member_1, source_code_span_matcher(span_1))
//
// but compiles much more quickly.
#define DIAG_TYPE_2_SPANS(type, member_0, span_0, member_1, span_1) \
  ::quick_lint_js::Diag_Spans_Matcher(                              \
      ::quick_lint_js::Diag_Type::type,                             \
      ::quick_lint_js::Diag_Spans_Matcher::Field{                   \
          DIAG_MATCHER_ARG(type, member_0),                         \
          span_0,                                                   \
      },                                                            \
      ::quick_lint_js::Diag_Spans_Matcher::Field{                   \
          DIAG_MATCHER_ARG(type, member_1),                         \
          span_1,                                                   \
      })

namespace quick_lint_js {
class Offsets_Matcher {
 public:
  // Create an Offsets_Matcher which asserts that the matched Source_Code_Span
  // begins at begin_offset and ends at end_offset.
  explicit Offsets_Matcher(Padded_String_View input,
                           CLI_Source_Position::Offset_Type begin_offset,
                           CLI_Source_Position::Offset_Type end_offset);

  // Create an Offsets_Matcher which asserts that the matched Source_Code_Span
  // begins at begin_offset and ends at begin_offset+strlen(text).
  //
  // TODO(strager): Also ensure the matched Source_Code_Span's content equals
  // text.
  explicit Offsets_Matcher(Padded_String_View input,
                           CLI_Source_Position::Offset_Type begin_offset,
                           String8_View text);

  Offsets_Matcher(const Offsets_Matcher &) = delete;
  Offsets_Matcher &operator=(const Offsets_Matcher &) = delete;

  Offsets_Matcher(Offsets_Matcher &&);
  Offsets_Matcher &operator=(Offsets_Matcher &&);

  ~Offsets_Matcher();

  /*implicit*/ operator testing::Matcher<const Source_Code_Span &>() const;

 private:
  class Span_Impl;

  Padded_String_View code_;
  CLI_Source_Position::Offset_Type begin_offset_;
  CLI_Source_Position::Offset_Type end_offset_;
};

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

// A mix of ::testing::VariantWith, ::testing::Field, and Offsets_Matcher. These
// are combined into one matcher to significantly reduce compile times.
//
// See DIAG_TYPE and DIAG_TYPE_OFFSETS for example usage.
class Diag_Matcher {
 public:
  struct Field {
    // Must be Source_Code_Span.
    Diag_Matcher_Arg arg;

    CLI_Source_Position::Offset_Type begin_offset;
    String8_View text;
  };

  explicit Diag_Matcher(Diag_Type type);

  // Create an Offsets_Matcher which asserts that an error's Source_Code_Span
  // begins at field.begin_offset and ends at
  // field.begin_offset+strlen(field.text).
  //
  // TODO(strager): Also ensure the error's Source_Code_Span's content equals
  // text.
  explicit Diag_Matcher(Padded_String_View input, Diag_Type type, Field);
  explicit Diag_Matcher(Padded_String_View input, Diag_Type type, Field, Field);
  explicit Diag_Matcher(Padded_String_View input, Diag_Type type, Field, Field,
                        Field);

  Diag_Matcher(const Diag_Matcher &) = default;
  Diag_Matcher(Diag_Matcher &&) = default;
  Diag_Matcher &operator=(const Diag_Matcher &) = default;
  Diag_Matcher &operator=(Diag_Matcher &&) = default;

  /*implicit*/ operator testing::Matcher<const Diag_Collector::Diag &>() const;

 private:
  class Impl;

  struct State {
    Diag_Type type;
    std::optional<Padded_String_View> input;
    std::vector<Field> fields;
  };

  State state_;
};

struct Any_Diag_Pointer {
  Diag_Type type;
  const void *data;
};

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

  /*implicit*/ operator testing::Matcher<const Diag_Collector::Diag &>() const;
  /*implicit*/ operator testing::Matcher<const Any_Diag_Pointer &>() const;

 private:
  class Impl;

  struct State {
    Diag_Type type;
    Padded_String_View input;
    std::vector<Field> fields;
  };

  State state_;
};

// A mix of ::testing::VariantWith, ::testing::Field, and
// source_code_span_matcher. These are combined into one matcher to
// significantly reduce compile times.
//
// See DIAG_TYPE_SPAN for example usage.
class Diag_Spans_Matcher {
 public:
  struct Field {
    // Must be Source_Code_Span.
    Diag_Matcher_Arg arg;

    Source_Code_Span expected;
  };

  // Create a matcher which asserts that an error's Source_Code_Span's
  // begin and end pointers equal the expected span.
  explicit Diag_Spans_Matcher(Diag_Type type, Field);
  explicit Diag_Spans_Matcher(Diag_Type type, Field, Field);

  Diag_Spans_Matcher(const Diag_Spans_Matcher &) = default;
  Diag_Spans_Matcher(Diag_Spans_Matcher &&) = default;
  Diag_Spans_Matcher &operator=(const Diag_Spans_Matcher &) = default;
  Diag_Spans_Matcher &operator=(Diag_Spans_Matcher &&) = default;

  /*implicit*/ operator testing::Matcher<const Diag_Collector::Diag &>() const;

 private:
  class Impl;

  struct State {
    Diag_Type type;
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
