// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <gmock/gmock.h>
#include <quick-lint-js/error-matcher.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/location.h>
#include <variant>

namespace quick_lint_js {
struct offsets_matcher::state {
  std::variant<const cli_locator *, padded_string_view> locator;
  cli_source_position::offset_type begin_offset;
  cli_source_position::offset_type end_offset;

  cli_source_range range(source_code_span span) const {
    struct range_impl {
      const source_code_span &span;

      cli_source_range operator()(const cli_locator *locator) const {
        return locator->range(this->span);
      }

      cli_source_range operator()(padded_string_view input) const {
        return cli_locator(input).range(this->span);
      }
    };
    return std::visit(range_impl{span}, this->locator);
  }
};

class offsets_matcher::span_impl
    : public testing::MatcherInterface<const source_code_span &> {
 public:
  explicit span_impl(state s) : state_(s) {}

  void DescribeTo(std::ostream *out) const override {
    *out << "has begin-end offset " << this->state_.begin_offset << '-'
         << this->state_.end_offset;
  }

  void DescribeNegationTo(std::ostream *out) const override {
    *out << "doesn't have begin-end offset " << this->state_.begin_offset << '-'
         << this->state_.end_offset;
  }

  bool MatchAndExplain(const source_code_span &span,
                       testing::MatchResultListener *listener) const override {
    cli_source_range range = this->state_.range(span);
    bool result = range.begin_offset() == this->state_.begin_offset &&
                  range.end_offset() == this->state_.end_offset;
    *listener << "whose begin-end offset (" << range.begin_offset() << '-'
              << range.end_offset() << ") "
              << (result ? "equals" : "doesn't equal") << " "
              << this->state_.begin_offset << '-' << this->state_.end_offset;
    return result;
  }

 private:
  state state_;
};

class offsets_matcher::identifier_impl
    : public testing::MatcherInterface<const identifier &> {
 public:
  explicit identifier_impl(state s) : impl_(s) {}

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
    : state_(new state{.locator = input,
                       .begin_offset = begin_offset,
                       .end_offset = end_offset}) {}

offsets_matcher::offsets_matcher(const cli_locator &locator,
                                 cli_source_position::offset_type begin_offset,
                                 cli_source_position::offset_type end_offset)
    : state_(new state{.locator = &locator,
                       .begin_offset = begin_offset,
                       .end_offset = end_offset}) {}

offsets_matcher::~offsets_matcher() = default;

/*implicit*/ offsets_matcher::operator testing::Matcher<const identifier &>()
    const {
  return testing::Matcher<const identifier &>(
      new identifier_impl(*this->state_));
}
/*implicit*/ offsets_matcher::operator testing::Matcher<
    const source_code_span &>() const {
  return testing::Matcher<const source_code_span &>(
      new span_impl(*this->state_));
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
}
