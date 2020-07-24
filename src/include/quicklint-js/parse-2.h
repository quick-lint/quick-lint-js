// quicklint-js finds bugs in JavaScript programs.
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

#ifndef QUICKLINT_JS_PARSE_2_H
#define QUICKLINT_JS_PARSE_2_H

#include <cstdlib>
#include <deque>
#include <optional>
#include <quicklint-js/error.h>
#include <quicklint-js/lex.h>
#include <quicklint-js/location.h>
#include <type_traits>
#include <vector>

namespace quicklint_js {
class expression;

class expression_ptr {
 public:
  explicit expression_ptr(const expression *ptr) noexcept : ptr_(ptr) {}

  const expression *operator->() const noexcept { return this->ptr_; }
  const expression &operator*() const noexcept { return *this->ptr_; }

  bool operator==(expression_ptr other) const noexcept {
    return this->ptr_ == other.ptr_;
  }

  bool operator!=(expression_ptr other) const noexcept {
    return !(*this == other);
  }

 private:
  const expression *ptr_;
};

enum class expression_kind {
  assignment,
  binary_operator,
  call,
  dot,
  literal,
  unary_operator,
  variable,
};

class expression {
 public:
  template <expression_kind Kind>
  struct tag {};

  explicit expression(tag<expression_kind::assignment>, expression_ptr lhs,
                      expression_ptr rhs) noexcept
      : kind_(expression_kind::assignment), children_{lhs, rhs} {}

  explicit expression(tag<expression_kind::binary_operator>,
                      std::vector<expression_ptr> &&children) noexcept
      : kind_(expression_kind::binary_operator),
        children_(std::move(children)) {}

  explicit expression(tag<expression_kind::call>,
                      std::vector<expression_ptr> &&children,
                      source_code_span span) noexcept
      : kind_(expression_kind::call),
        call_right_paren_end_(span.end()),
        children_(std::move(children)) {}

  explicit expression(tag<expression_kind::dot>, expression_ptr lhs,
                      identifier rhs) noexcept
      : kind_(expression_kind::dot),
        variable_identifier_(rhs),
        children_{lhs} {}

  explicit expression(tag<expression_kind::literal>,
                      source_code_span span) noexcept
      : kind_(expression_kind::literal), literal_span_(span) {}

  explicit expression(tag<expression_kind::unary_operator>,
                      expression_ptr child,
                      source_code_span operator_span) noexcept
      : kind_(expression_kind::unary_operator),
        unary_operator_begin_(operator_span.begin()),
        children_{child} {}

  explicit expression(tag<expression_kind::variable>,
                      identifier variable_identifier) noexcept
      : kind_(expression_kind::variable),
        variable_identifier_(variable_identifier) {}

  expression_kind kind() const noexcept { return this->kind_; }

  identifier variable_identifier() const noexcept {
    switch (this->kind_) {
      case expression_kind::dot:
      case expression_kind::variable:
        break;
      default:
        assert(false);
        break;
    }
    return this->variable_identifier_;
  }

  int child_count() const noexcept {
    switch (this->kind_) {
      case expression_kind::assignment:
      case expression_kind::binary_operator:
      case expression_kind::call:
        break;
      default:
        assert(false);
        break;
    }
    return this->children_.size();
  }

  expression_ptr child_0() const noexcept { return this->child(0); }

  expression_ptr child_1() const noexcept { return this->child(1); }

  expression_ptr child(int index) const noexcept {
    assert(index >= 0);
    assert(index < static_cast<int>(this->children_.size()));
    return this->children_[index];
  }

  source_code_span span() const noexcept {
    switch (this->kind_) {
      case expression_kind::assignment:
      case expression_kind::binary_operator:
        return source_code_span(this->children_.front()->span().begin(),
                                this->children_.back()->span().end());
      case expression_kind::call:
        return source_code_span(this->children_.front()->span().begin(),
                                this->call_right_paren_end_);
      case expression_kind::dot:
        return source_code_span(this->child_0()->span().begin(),
                                this->variable_identifier_.span().end());
      case expression_kind::literal:
        return this->literal_span_;
      case expression_kind::unary_operator:
        return source_code_span(this->unary_operator_begin_,
                                this->child_0()->span().end());
      case expression_kind::variable:
        return this->variable_identifier_.span();
    }
    __builtin_unreachable();
  }

 private:
  expression_kind kind_;
  union {
    identifier variable_identifier_;  // dot or variable
    static_assert(std::is_trivially_destructible_v<identifier>);

    const char *call_right_paren_end_;  // call

    source_code_span literal_span_;  // literal
    static_assert(std::is_trivially_destructible_v<source_code_span>);

    const char *unary_operator_begin_;  // unary_operator
  };
  std::vector<expression_ptr> children_;
};

class parser2 {
 public:
  explicit parser2(const char *input, error_reporter *error_reporter)
      : lexer_(input, error_reporter),
        locator_(input),
        error_reporter_(error_reporter) {}

  const quicklint_js::locator &locator() const noexcept {
    return this->locator_;
  }

  expression_ptr parse_expression() {
    return this->parse_expression(precedence{.binary_operators = true});
  }

 private:
  struct precedence {
    bool binary_operators;
  };

  expression_ptr parse_expression(precedence prec);

  expression_ptr parse_expression_remainder(expression_ptr);

  const token &peek() const noexcept { return this->lexer_.peek(); }

  [[noreturn]] void crash_on_unimplemented_token(
      const char *qljs_file_name, int qljs_line,
      const char *qljs_function_name);

  template <expression_kind Kind, class... Args>
  expression_ptr make_expression(Args &&... args) {
    this->expressions_.emplace_back(expression::tag<Kind>(),
                                    std::forward<Args>(args)...);
    return expression_ptr(&this->expressions_.back());
  }

  lexer lexer_;
  quicklint_js::locator locator_;
  error_reporter *error_reporter_;

  std::deque<expression> expressions_;
};
}  // namespace quicklint_js

#endif
