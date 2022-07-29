// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_DIAG_MATCHER_H
#define QUICK_LINT_JS_DIAG_MATCHER_H

#include <cstddef>
#include <gmock/gmock.h>
#include <optional>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/fe/diagnostic-types.h>
#include <quick-lint-js/fe/diagnostic.h>
#include <quick-lint-js/fe/identifier.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/port/char8.h>
#include <vector>

#define DIAG_TYPE_FIELD(diag_type, member, matcher) \
  ::testing::VariantWith<diag_type>(                \
      ::testing::Field(#member, &diag_type::member, matcher))

#define DIAG_TYPE_2_FIELDS(diag_type, member_1, matcher_1, member_2, \
                           matcher_2)                                \
  ::testing::VariantWith<diag_type>(::testing::AllOf(                \
      ::testing::Field(#member_1, &diag_type::member_1, matcher_1),  \
      ::testing::Field(#member_2, &diag_type::member_2, matcher_2)))

#define DIAG_TYPE_3_FIELDS(diag_type, member_1, matcher_1, member_2, \
                           matcher_2, member_3, matcher_3)           \
  ::testing::VariantWith<diag_type>(::testing::AllOf(                \
      ::testing::Field(#member_1, &diag_type::member_1, matcher_1),  \
      ::testing::Field(#member_2, &diag_type::member_2, matcher_2),  \
      ::testing::Field(#member_3, &diag_type::member_3, matcher_3)))

// Equivalent to ::testing::VariantWith<type>(::testing::_), but compiles much
// more quickly.
#define DIAG_TYPE(type) \
  ::quick_lint_js::diag_matcher(::quick_lint_js::diag_type::type)

// Equivalent to the following, but compiles much more quickly:
//
//   DIAG_TYPE_FIELD(type, member_0,
//                    offsets_matcher(code, start_0, end_or_text_0))
//
// but compiles much more quickly.
#define DIAG_TYPE_OFFSETS(code, type, member_0, start_0, end_or_text_0) \
  ::quick_lint_js::diag_matcher(code, ::quick_lint_js::diag_type::type, \
                                ::quick_lint_js::diag_matcher::field{   \
                                    DIAG_MATCHER_ARG(type, member_0),   \
                                    start_0,                            \
                                    end_or_text_0,                      \
                                })

// Equivalent to the following, but compiles much more quickly:
//
//   DIAG_TYPE_FIELD(type,
//                    member_0, offsets_matcher(code, start_0, end_or_text_0),
//                    member_1, offsets_matcher(code, start_1, end_or_text_1))
#define DIAG_TYPE_2_OFFSETS(code, type, member_0, start_0, end_or_text_0, \
                            member_1, start_1, end_or_text_1)             \
  ::quick_lint_js::diag_matcher(code, ::quick_lint_js::diag_type::type,   \
                                ::quick_lint_js::diag_matcher::field{     \
                                    DIAG_MATCHER_ARG(type, member_0),     \
                                    start_0,                              \
                                    end_or_text_0,                        \
                                },                                        \
                                ::quick_lint_js::diag_matcher::field{     \
                                    DIAG_MATCHER_ARG(type, member_1),     \
                                    start_1,                              \
                                    end_or_text_1,                        \
                                })

// Equivalent to the following, but compiles much more quickly:
//
//   DIAG_TYPE_FIELD(type,
//                    member_0, offsets_matcher(code, start_0, end_or_text_0),
//                    member_1, offsets_matcher(code, start_1, end_or_text_1),
//                    member_2, offsets_matcher(code, start_2, end_or_text_2))
#define DIAG_TYPE_3_OFFSETS(code, type, member_0, start_0, end_or_text_0, \
                            member_1, start_1, end_or_text_1, member_2,   \
                            start_2, end_or_text_2)                       \
  ::quick_lint_js::diag_matcher(code, ::quick_lint_js::diag_type::type,   \
                                ::quick_lint_js::diag_matcher::field{     \
                                    DIAG_MATCHER_ARG(type, member_0),     \
                                    start_0,                              \
                                    end_or_text_0,                        \
                                },                                        \
                                ::quick_lint_js::diag_matcher::field{     \
                                    DIAG_MATCHER_ARG(type, member_1),     \
                                    start_1,                              \
                                    end_or_text_1,                        \
                                },                                        \
                                ::quick_lint_js::diag_matcher::field{     \
                                    DIAG_MATCHER_ARG(type, member_2),     \
                                    start_2,                              \
                                    end_or_text_2,                        \
                                })

// Equivalent to the following, but compiles much more quickly:
//
//   DIAG_TYPE_FIELD(type, member_0, source_code_span_matcher(span_0))
//
// but compiles much more quickly.
#define DIAG_TYPE_SPAN(type, member_0, span_0)    \
  ::quick_lint_js::diag_spans_matcher(            \
      ::quick_lint_js::diag_type::type,           \
      ::quick_lint_js::diag_spans_matcher::field{ \
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
  ::quick_lint_js::diag_spans_matcher(                              \
      ::quick_lint_js::diag_type::type,                             \
      ::quick_lint_js::diag_spans_matcher::field{                   \
          DIAG_MATCHER_ARG(type, member_0),                         \
          span_0,                                                   \
      },                                                            \
      ::quick_lint_js::diag_spans_matcher::field{                   \
          DIAG_MATCHER_ARG(type, member_1),                         \
          span_1,                                                   \
      })

namespace quick_lint_js {
class offsets_matcher {
 public:
  // Create an offsets_matcher which asserts that the matched source_code_span
  // begins at begin_offset and ends at end_offset.
  explicit offsets_matcher(padded_string_view input,
                           cli_source_position::offset_type begin_offset,
                           cli_source_position::offset_type end_offset);

  // Create an offsets_matcher which asserts that the matched source_code_span
  // begins at begin_offset and ends at begin_offset+strlen(text).
  //
  // TODO(strager): Also ensure the matched source_code_span's content equals
  // text.
  explicit offsets_matcher(padded_string_view input,
                           cli_source_position::offset_type begin_offset,
                           string8_view text);

  offsets_matcher(const offsets_matcher &) = delete;
  offsets_matcher &operator=(const offsets_matcher &) = delete;

  offsets_matcher(offsets_matcher &&);
  offsets_matcher &operator=(offsets_matcher &&);

  ~offsets_matcher();

  /*implicit*/ operator testing::Matcher<const identifier &>() const;
  /*implicit*/ operator testing::Matcher<const source_code_span &>() const;

 private:
  class identifier_impl;
  class span_impl;

  padded_string_view code_;
  cli_source_position::offset_type begin_offset_;
  cli_source_position::offset_type end_offset_;
};

// Like source_code_span_matcher, but only check the begin pointer.
//
// TODO(strager): Delete in favor of source_code_span_matcher.
class span_matcher {
 public:
  explicit span_matcher(const char8 *expected);

  /*implicit*/ operator testing::Matcher<const identifier &>() const;
  /*implicit*/ operator testing::Matcher<const source_code_span &>() const;

 private:
  class identifier_impl;
  class span_impl;

  const char8 *expected_;
};

class source_code_span_matcher {
 public:
  explicit source_code_span_matcher(source_code_span expected);

  /*implicit*/ operator testing::Matcher<const identifier &>() const;
  /*implicit*/ operator testing::Matcher<const source_code_span &>() const;

 private:
  class identifier_impl;
  class span_impl;

  source_code_span expected_;
};

// Metadata for a member of a diagnostic class.
struct diag_matcher_arg {
  const char *member_name;
  std::size_t member_offset;
  diagnostic_arg_type member_type;

  // member_type must be identifier or source_code_span.
  source_code_span get_span(const void *error_object) const noexcept;
};

// Create a diag_matcher_arg from a diag_ struct type and the name of a member
// of that struct.
#define DIAG_MATCHER_ARG(type, member)                           \
  (::quick_lint_js::diag_matcher_arg{                            \
      #member,                                                   \
      offsetof(type, member),                                    \
      ::quick_lint_js::get_diagnostic_message_arg_type<decltype( \
          type::member)>(),                                      \
  })

// A mix of ::testing::VariantWith, ::testing::Field, and offsets_matcher. These
// are combined into one matcher to significantly reduce compile times.
//
// See DIAG_TYPE and DIAG_TYPE_OFFSETS for example usage.
class diag_matcher {
 public:
  struct field {
    // Must be identifier or source_code_span.
    diag_matcher_arg arg;

    cli_source_position::offset_type begin_offset;
    string8_view text;
  };

  explicit diag_matcher(diag_type type);

  // Create an offsets_matcher which asserts that an error's source_code_span
  // begins at field.begin_offset and ends at
  // field.begin_offset+strlen(field.text).
  //
  // TODO(strager): Also ensure the error's source_code_span's content equals
  // text.
  explicit diag_matcher(padded_string_view input, diag_type type, field);
  explicit diag_matcher(padded_string_view input, diag_type type, field, field);
  explicit diag_matcher(padded_string_view input, diag_type type, field, field,
                        field);

  diag_matcher(const diag_matcher &) = default;
  diag_matcher(diag_matcher &&) = default;
  diag_matcher &operator=(const diag_matcher &) = default;
  diag_matcher &operator=(diag_matcher &&) = default;

  /*implicit*/ operator testing::Matcher<const diag_collector::diag &>() const;

 private:
  class impl;

  struct state {
    diag_type type;
    std::optional<padded_string_view> input;
    std::vector<field> fields;
  };

  state state_;
};

// A mix of ::testing::VariantWith, ::testing::Field, and
// source_code_span_matcher. These are combined into one matcher to
// significantly reduce compile times.
//
// See DIAG_TYPE_SPAN for example usage.
class diag_spans_matcher {
 public:
  struct field {
    // Must be identifier or source_code_span.
    diag_matcher_arg arg;

    source_code_span expected;
  };

  // Create a matcher which asserts that an error's source_code_span's
  // begin and end pointers equal the expected span.
  explicit diag_spans_matcher(diag_type type, field);
  explicit diag_spans_matcher(diag_type type, field, field);

  diag_spans_matcher(const diag_spans_matcher &) = default;
  diag_spans_matcher(diag_spans_matcher &&) = default;
  diag_spans_matcher &operator=(const diag_spans_matcher &) = default;
  diag_spans_matcher &operator=(diag_spans_matcher &&) = default;

  /*implicit*/ operator testing::Matcher<const diag_collector::diag &>() const;

 private:
  class impl;

  struct state {
    diag_type type;
    std::vector<field> fields;
  };

  state state_;
};
}

#endif

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
