// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <gmock/gmock.h>
#include <optional>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/fe/diagnostic-types.h>
#include <quick-lint-js/fe/lex.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/port/unreachable.h>
#include <vector>

namespace quick_lint_js {
class offsets_matcher::span_impl
    : public testing::MatcherInterface<const source_code_span &> {
 public:
  explicit span_impl(padded_string_view code,
                     cli_source_position::offset_type begin_offset,
                     cli_source_position::offset_type end_offset)
      : code_(code), begin_offset_(begin_offset), end_offset_(end_offset) {}

  void DescribeTo(std::ostream *out) const override {
    *out << "has begin-end offset " << this->begin_offset_ << '-'
         << this->end_offset_;
  }

  void DescribeNegationTo(std::ostream *out) const override {
    *out << "doesn't have begin-end offset " << this->begin_offset_ << '-'
         << this->end_offset_;
  }

  bool MatchAndExplain(const source_code_span &span,
                       testing::MatchResultListener *listener) const override {
    auto span_begin_offset = narrow_cast<cli_source_position::offset_type>(
        span.begin() - this->code_.data());
    auto span_end_offset = narrow_cast<cli_source_position::offset_type>(
        span.end() - this->code_.data());
    bool result = span_begin_offset == this->begin_offset_ &&
                  span_end_offset == this->end_offset_;
    *listener << "whose begin-end offset (" << span_begin_offset << '-'
              << span_end_offset << ") "
              << (result ? "equals" : "doesn't equal") << " "
              << this->begin_offset_ << '-' << this->end_offset_;
    return result;
  }

 private:
  padded_string_view code_;
  cli_source_position::offset_type begin_offset_;
  cli_source_position::offset_type end_offset_;
};

class offsets_matcher::identifier_impl
    : public testing::MatcherInterface<const identifier &> {
 public:
  explicit identifier_impl(padded_string_view code,
                           cli_source_position::offset_type begin_offset,
                           cli_source_position::offset_type end_offset)
      : impl_(code, begin_offset, end_offset) {}

  void DescribeTo(std::ostream *out) const override {
    this->impl_.DescribeTo(out);
  }

  void DescribeNegationTo(std::ostream *out) const override {
    this->impl_.DescribeNegationTo(out);
  }

  bool MatchAndExplain(const identifier &ident,
                       testing::MatchResultListener *listener) const override {
    return this->impl_.MatchAndExplain(ident.span(), listener);
  }

 private:
  span_impl impl_;
};

offsets_matcher::offsets_matcher(padded_string_view input,
                                 cli_source_position::offset_type begin_offset,
                                 cli_source_position::offset_type end_offset)
    : code_(input), begin_offset_(begin_offset), end_offset_(end_offset) {}

offsets_matcher::offsets_matcher(padded_string_view input,
                                 cli_source_position::offset_type begin_offset,
                                 string8_view text)
    : code_(input),
      begin_offset_(begin_offset),
      end_offset_(begin_offset + text.size()) {}

offsets_matcher::offsets_matcher(offsets_matcher &&) = default;

offsets_matcher &offsets_matcher::operator=(offsets_matcher &&) = default;

offsets_matcher::~offsets_matcher() = default;

/*implicit*/ offsets_matcher::operator testing::Matcher<const identifier &>()
    const {
  return testing::Matcher<const identifier &>(
      new identifier_impl(this->code_, this->begin_offset_, this->end_offset_));
}

/*implicit*/ offsets_matcher::operator testing::Matcher<
    const source_code_span &>() const {
  return testing::Matcher<const source_code_span &>(
      new span_impl(this->code_, this->begin_offset_, this->end_offset_));
}

class span_matcher::span_impl
    : public testing::MatcherInterface<const source_code_span &> {
 public:
  explicit span_impl(const char8 *expected) : expected_(expected) {}

  void DescribeTo(std::ostream *out) const override {
    *out << "begins at " << static_cast<const void *>(this->expected_);
  }

  void DescribeNegationTo(std::ostream *out) const override {
    *out << "doesn't begin at " << static_cast<const void *>(this->expected_);
  }

  bool MatchAndExplain(const source_code_span &span,
                       testing::MatchResultListener *listener) const override {
    bool result = span.begin() == this->expected_;
    *listener << "whose span (at " << static_cast<const void *>(span.begin())
              << ") " << (result ? "begins" : "doesn't begin") << " at "
              << static_cast<const void *>(this->expected_);
    return result;
  }

 private:
  const char8 *expected_;
};

class span_matcher::identifier_impl
    : public testing::MatcherInterface<const identifier &> {
 public:
  explicit identifier_impl(const char8 *expected) : impl_(expected) {}

  void DescribeTo(std::ostream *out) const override {
    this->impl_.DescribeTo(out);
  }

  void DescribeNegationTo(std::ostream *out) const override {
    this->impl_.DescribeNegationTo(out);
  }

  bool MatchAndExplain(const identifier &ident,
                       testing::MatchResultListener *listener) const override {
    return this->impl_.MatchAndExplain(ident.span(), listener);
  }

 private:
  span_impl impl_;
};

span_matcher::span_matcher(const char8 *expected) : expected_(expected) {}

span_matcher::operator testing::Matcher<const identifier &>() const {
  return testing::Matcher<const identifier &>(
      new identifier_impl(this->expected_));
}

span_matcher::operator testing::Matcher<const source_code_span &>() const {
  return testing::Matcher<const source_code_span &>(
      new span_impl(this->expected_));
}

class source_code_span_matcher::span_impl
    : public testing::MatcherInterface<const source_code_span &> {
 public:
  explicit span_impl(source_code_span expected) : expected_(expected) {}

  void DescribeTo(std::ostream *out) const override {
    *out << "begins at " << static_cast<const void *>(this->expected_.begin())
         << " and ends at " << static_cast<const void *>(this->expected_.end());
  }

  void DescribeNegationTo(std::ostream *out) const override {
    *out << "doesn't begin at "
         << static_cast<const void *>(this->expected_.begin()) << " and end at "
         << static_cast<const void *>(this->expected_.end());
  }

  bool MatchAndExplain(const source_code_span &span,
                       testing::MatchResultListener *listener) const override {
    bool result = same_pointers(span, this->expected_);
    *listener << "whose span (from " << static_cast<const void *>(span.begin())
              << " to " << static_cast<const void *>(span.end()) << ") "
              << (result ? "equals" : "doesn't equal") << " expected (from "
              << static_cast<const void *>(this->expected_.begin()) << " to "
              << static_cast<const void *>(this->expected_.begin()) << ")";
    return result;
  }

 private:
  source_code_span expected_;
};

class source_code_span_matcher::identifier_impl
    : public testing::MatcherInterface<const identifier &> {
 public:
  explicit identifier_impl(source_code_span expected) : impl_(expected) {}

  void DescribeTo(std::ostream *out) const override {
    this->impl_.DescribeTo(out);
  }

  void DescribeNegationTo(std::ostream *out) const override {
    this->impl_.DescribeNegationTo(out);
  }

  bool MatchAndExplain(const identifier &ident,
                       testing::MatchResultListener *listener) const override {
    return this->impl_.MatchAndExplain(ident.span(), listener);
  }

 private:
  span_impl impl_;
};

source_code_span_matcher::source_code_span_matcher(source_code_span expected)
    : expected_(expected) {}

source_code_span_matcher::operator testing::Matcher<const identifier &>()
    const {
  return testing::Matcher<const identifier &>(
      new identifier_impl(this->expected_));
}

source_code_span_matcher::operator testing::Matcher<const source_code_span &>()
    const {
  return testing::Matcher<const source_code_span &>(
      new span_impl(this->expected_));
}

source_code_span diag_matcher_arg::get_span(const void *error_object) const
    noexcept {
  const void *member_data =
      reinterpret_cast<const char *>(error_object) + this->member_offset;
  switch (this->member_type) {
  case diagnostic_arg_type::identifier:
    return static_cast<const identifier *>(member_data)->span();
  case diagnostic_arg_type::source_code_span:
    return *static_cast<const source_code_span *>(member_data);

  case diagnostic_arg_type::char8:
  case diagnostic_arg_type::enum_kind:
  case diagnostic_arg_type::invalid:
  case diagnostic_arg_type::statement_kind:
  case diagnostic_arg_type::string8_view:
  case diagnostic_arg_type::variable_kind:
    QLJS_ASSERT(false && "invalid arg type");
    break;
  }
  QLJS_UNREACHABLE();
}

template <class State, class Field>
class diag_fields_matcher_impl_base
    : public testing::MatcherInterface<const diag_collector::diag &> {
 public:
  explicit diag_fields_matcher_impl_base(State s) : state_(std::move(s)) {}

  void DescribeTo(std::ostream *out) const override {
    *out << "has type " << this->state_.type;
    this->describe_fields_to(out);
  }

  void DescribeNegationTo(std::ostream *out) const override {
    *out << "doesn't have type " << this->state_.type;
    this->describe_fields_to(out);
  }

  void describe_fields_to(std::ostream *) const {
    // TODO(strager)
  }

  bool MatchAndExplain(const diag_collector::diag &error,
                       testing::MatchResultListener *listener) const override {
    bool type_matches = error.type() == this->state_.type;
    if (!type_matches) {
      *listener << "whose type (" << error.type() << ") isn't "
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
  virtual bool field_matches(const diag_collector::diag &error, const Field &f,
                             testing::MatchResultListener *listener) const = 0;

  State state_;
};

diag_matcher::diag_matcher(diag_type type) : state_{type, std::nullopt, {}} {}

diag_matcher::diag_matcher(padded_string_view input, diag_type type,
                           field field_0)
    : state_{type, input, {field_0}} {}

diag_matcher::diag_matcher(padded_string_view input, diag_type type,
                           field field_0, field field_1)
    : state_{type, input, {field_0, field_1}} {}

diag_matcher::diag_matcher(padded_string_view input, diag_type type,
                           field field_0, field field_1, field field_2)
    : state_{type, input, {field_0, field_1, field_2}} {}

class diag_matcher::impl
    : public diag_fields_matcher_impl_base<diag_matcher::state,
                                           diag_matcher::field> {
 public:
  using base =
      diag_fields_matcher_impl_base<diag_matcher::state, diag_matcher::field>;

  using base::base;

 protected:
  bool field_matches(const diag_collector::diag &error, const field &f,
                     testing::MatchResultListener *listener) const override {
    QLJS_ASSERT(this->state_.input.has_value());
    source_code_span span = f.arg.get_span(error.data());
    auto span_begin_offset = narrow_cast<cli_source_position::offset_type>(
        span.begin() - this->state_.input->data());
    auto span_end_offset = narrow_cast<cli_source_position::offset_type>(
        span.end() - this->state_.input->data());
    auto expected_end_offset = f.begin_offset + f.text.size();

    bool span_matches = span_begin_offset == f.begin_offset &&
                        span_end_offset == expected_end_offset;
    *listener << "whose ." << f.arg.member_name << " (" << span_begin_offset
              << "-" << span_end_offset << ") "
              << (span_matches ? "equals" : "doesn't equal") << " "
              << f.begin_offset << "-" << expected_end_offset;
    return span_matches;
  }
};

/*implicit*/ diag_matcher::operator testing::Matcher<
    const diag_collector::diag &>() const {
  return testing::Matcher<const diag_collector::diag &>(new impl(this->state_));
}

diag_spans_matcher::diag_spans_matcher(diag_type type, field field_0)
    : state_{type, {field_0}} {}

diag_spans_matcher::diag_spans_matcher(diag_type type, field field_0,
                                       field field_1)
    : state_{type, {field_0, field_1}} {}

class diag_spans_matcher::impl
    : public diag_fields_matcher_impl_base<diag_spans_matcher::state,
                                           diag_spans_matcher::field> {
 public:
  using base = diag_fields_matcher_impl_base<diag_spans_matcher::state,
                                             diag_spans_matcher::field>;

  using base::base;

 protected:
  bool field_matches(const diag_collector::diag &error, const field &f,
                     testing::MatchResultListener *listener) const override {
    source_code_span span = f.arg.get_span(error.data());
    bool span_matches = same_pointers(span, f.expected);
    *listener << "whose ." << f.arg.member_name << " (`"
              << out_string8(span.string_view()) << "` @"
              << reinterpret_cast<const void *>(span.begin()) << ") "
              << (span_matches ? "equals" : "doesn't equal") << " `"
              << out_string8(f.expected.string_view()) << "` @"
              << reinterpret_cast<const void *>(f.expected.begin());
    return span_matches;
  }
};

/*implicit*/ diag_spans_matcher::operator testing::Matcher<
    const diag_collector::diag &>() const {
  return testing::Matcher<const diag_collector::diag &>(new impl(this->state_));
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
