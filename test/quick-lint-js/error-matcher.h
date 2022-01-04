// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_ERROR_MATCHER_H
#define QUICK_LINT_JS_ERROR_MATCHER_H

#include <cstddef>
#include <gmock/gmock.h>
#include <optional>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/padded-string.h>
#include <vector>

#define ERROR_TYPE_FIELD(error_type, member, matcher) \
  ::testing::VariantWith<error_type>(                 \
      ::testing::Field(#member, &error_type::member, matcher))

#define ERROR_TYPE_2_FIELDS(error_type, member_1, matcher_1, member_2, \
                            matcher_2)                                 \
  ::testing::VariantWith<error_type>(::testing::AllOf(                 \
      ::testing::Field(#member_1, &error_type::member_1, matcher_1),   \
      ::testing::Field(#member_2, &error_type::member_2, matcher_2)))

#define ERROR_TYPE_3_FIELDS(error_type, member_1, matcher_1, member_2, \
                            matcher_2, member_3, matcher_3)            \
  ::testing::VariantWith<error_type>(::testing::AllOf(                 \
      ::testing::Field(#member_1, &error_type::member_1, matcher_1),   \
      ::testing::Field(#member_2, &error_type::member_2, matcher_2),   \
      ::testing::Field(#member_3, &error_type::member_3, matcher_3)))

// Equivalent to ::testing::VariantWith<type>(::testing::_), but compiles much
// more quickly.
#define ERROR_TYPE(type) \
  ::quick_lint_js::error_matcher(::quick_lint_js::error_type::type)

// Equivalent to the following, but compiles much more quickly:
//
//   ERROR_TYPE_FIELD(type, member_0,
//                    offsets_matcher(code, start_0, end_or_text_0))
//
// but compiles much more quickly.
#define ERROR_TYPE_OFFSETS(code, type, member_0, start_0, end_or_text_0) \
  ::quick_lint_js::error_matcher(                                        \
      code, ::quick_lint_js::error_type::type,                           \
      ::quick_lint_js::error_matcher::field{                             \
          #member_0,                                                     \
          offsetof(type, member_0),                                      \
          ::quick_lint_js::get_error_matcher_field_type<decltype(        \
              type::member_0)>(),                                        \
          start_0,                                                       \
          end_or_text_0,                                                 \
      })

// Equivalent to the following, but compiles much more quickly:
//
//   ERROR_TYPE_FIELD(type,
//                    member_0, offsets_matcher(code, start_0, end_or_text_0),
//                    member_1, offsets_matcher(code, start_1, end_or_text_1))
#define ERROR_TYPE_2_OFFSETS(code, type, member_0, start_0, end_or_text_0, \
                             member_1, start_1, end_or_text_1)             \
  ::quick_lint_js::error_matcher(                                          \
      code, ::quick_lint_js::error_type::type,                             \
      ::quick_lint_js::error_matcher::field{                               \
          #member_0,                                                       \
          offsetof(type, member_0),                                        \
          ::quick_lint_js::get_error_matcher_field_type<decltype(          \
              type::member_0)>(),                                          \
          start_0,                                                         \
          end_or_text_0,                                                   \
      },                                                                   \
      ::quick_lint_js::error_matcher::field{                               \
          #member_1,                                                       \
          offsetof(type, member_1),                                        \
          ::quick_lint_js::get_error_matcher_field_type<decltype(          \
              type::member_1)>(),                                          \
          start_1,                                                         \
          end_or_text_1,                                                   \
      })

// Equivalent to the following, but compiles much more quickly:
//
//   ERROR_TYPE_FIELD(type,
//                    member_0, offsets_matcher(code, start_0, end_or_text_0),
//                    member_1, offsets_matcher(code, start_1, end_or_text_1),
//                    member_2, offsets_matcher(code, start_2, end_or_text_2))
#define ERROR_TYPE_3_OFFSETS(code, type, member_0, start_0, end_or_text_0, \
                             member_1, start_1, end_or_text_1, member_2,   \
                             start_2, end_or_text_2)                       \
  ::quick_lint_js::error_matcher(                                          \
      code, ::quick_lint_js::error_type::type,                             \
      ::quick_lint_js::error_matcher::field{                               \
          #member_0,                                                       \
          offsetof(type, member_0),                                        \
          ::quick_lint_js::get_error_matcher_field_type<decltype(          \
              type::member_0)>(),                                          \
          start_0,                                                         \
          end_or_text_0,                                                   \
      },                                                                   \
      ::quick_lint_js::error_matcher::field{                               \
          #member_1,                                                       \
          offsetof(type, member_1),                                        \
          ::quick_lint_js::get_error_matcher_field_type<decltype(          \
              type::member_1)>(),                                          \
          start_1,                                                         \
          end_or_text_1,                                                   \
      },                                                                   \
      ::quick_lint_js::error_matcher::field{                               \
          #member_2,                                                       \
          offsetof(type, member_2),                                        \
          ::quick_lint_js::get_error_matcher_field_type<decltype(          \
              type::member_2)>(),                                          \
          start_2,                                                         \
          end_or_text_2,                                                   \
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

// A mix of ::testing::VariantWith, ::testing::Field, and offsets_matcher. These
// are combined into one matcher to significantly reduce compile times.
//
// See ERROR_TYPE and ERROR_TYPE_OFFSETS for example usage.
class error_matcher {
 public:
  enum class field_type {
    identifier,
    source_code_span,
  };

  struct field {
    const char *member_name;
    std::size_t member_offset;
    field_type member_type;

    cli_source_position::offset_type begin_offset;
    string8_view text;

    source_code_span get_span(const void *error_object) const noexcept;
  };

  explicit error_matcher(error_type type);

  // Create an offsets_matcher which asserts that an error's source_code_span
  // begins at field.begin_offset and ends at
  // field.begin_offset+strlen(field.text).
  //
  // TODO(strager): Also ensure the error's source_code_span's content equals
  // text.
  explicit error_matcher(padded_string_view input, error_type type, field);
  explicit error_matcher(padded_string_view input, error_type type, field,
                         field);
  explicit error_matcher(padded_string_view input, error_type type, field,
                         field, field);

  error_matcher(const error_matcher &) = default;
  error_matcher(error_matcher &&) = default;
  error_matcher &operator=(const error_matcher &) = default;
  error_matcher &operator=(error_matcher &&) = default;

  /*implicit*/ operator testing::Matcher<const error_collector::error &>()
      const;

 private:
  class impl;

  struct state {
    error_type type;
    std::optional<padded_string_view> input;
    std::vector<field> fields;
  };

  state state_;
};

// Internal helper for ERROR_TYPE_OFFSETS macros.
template <class>
constexpr error_matcher::field_type get_error_matcher_field_type() noexcept =
    delete;
template <>
constexpr error_matcher::field_type
get_error_matcher_field_type<identifier>() noexcept {
  return error_matcher::field_type::identifier;
}
template <>
constexpr error_matcher::field_type
get_error_matcher_field_type<source_code_span>() noexcept {
  return error_matcher::field_type::source_code_span;
}
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
