// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_ERROR_MATCHER_H
#define QUICK_LINT_JS_ERROR_MATCHER_H

#include <gmock/gmock.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/padded-string.h>

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
