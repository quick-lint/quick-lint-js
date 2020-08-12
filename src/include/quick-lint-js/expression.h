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

#ifndef QUICK_LINT_JS_EXPRESSION_H
#define QUICK_LINT_JS_EXPRESSION_H

#include <array>
#include <deque>
#include <memory>
#include <optional>
#include <quick-lint-js/buffering-visitor.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/unreachable.h>
#include <type_traits>
#include <utility>
#include <vector>

#define QLJS_UNEXPECTED_EXPRESSION_KIND()                                 \
  do {                                                                    \
    assert(false && "function not implemented for this expression kind"); \
    QLJS_UNREACHABLE();                                                   \
  } while (false)

namespace quick_lint_js {
class expression;

class expression_ptr {
 public:
  explicit expression_ptr(expression *ptr) noexcept : ptr_(ptr) {}

  expression *operator->() const noexcept { return this->ptr_; }
  expression &operator*() const noexcept { return *this->ptr_; }

  bool operator==(expression_ptr other) const noexcept {
    return this->ptr_ == other.ptr_;
  }

  bool operator!=(expression_ptr other) const noexcept {
    return !(*this == other);
  }

 private:
  expression *ptr_;
};

enum class expression_kind {
  _invalid,
  _new,
  _template,
  array,
  arrow_function_with_expression,
  arrow_function_with_statements,
  assignment,
  await,
  binary_operator,
  call,
  compound_assignment,
  conditional,
  dot,
  function,
  import,
  index,
  literal,
  named_function,
  object,
  rw_unary_prefix,
  rw_unary_suffix,
  spread,
  super,
  unary_operator,
  variable,
};

class expression {
 public:
  class expression_with_prefix_operator_base;

  class _invalid;
  class _new;
  class _template;
  class array;
  class arrow_function_with_expression;
  class arrow_function_with_statements;
  class assignment;
  class await;
  class binary_operator;
  class call;
  class conditional;
  class dot;
  class function;
  class import;
  class index;
  class literal;
  class named_function;
  class object;
  class rw_unary_prefix;
  class rw_unary_suffix;
  class spread;
  class super;
  class unary_operator;
  class variable;

  struct object_property_value_pair {
    explicit object_property_value_pair(expression_ptr property,
                                        expression_ptr value) noexcept
        : property(property), value(value) {}

    explicit object_property_value_pair(expression_ptr value) noexcept
        : property(std::nullopt), value(value) {}

    std::optional<expression_ptr> property;
    expression_ptr value;
  };

  virtual ~expression() = default;

  expression_kind kind() const noexcept { return this->kind_; }

  virtual identifier variable_identifier() const noexcept {
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }

  virtual int child_count() const noexcept {
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }

  expression_ptr child_0() const noexcept { return this->child(0); }
  expression_ptr child_1() const noexcept { return this->child(1); }
  expression_ptr child_2() const noexcept { return this->child(2); }

  virtual expression_ptr child(int) const noexcept {
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }

  // Can be called at most once.
  template <class Visitor>
  void visit_children(Visitor &v) {
    std::unique_ptr<buffering_visitor> child_visits = this->take_child_visits();
    assert(
        child_visits &&
        "visit_children can be called at most once, but it was called twice");
    child_visits->move_into(v);
  }

  virtual int object_entry_count() const noexcept {
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }

  virtual object_property_value_pair object_entry(int) const noexcept {
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }

  virtual source_code_span span() const noexcept {
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }

  virtual function_attributes attributes() const noexcept {
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }

 protected:
  explicit expression(expression_kind kind) noexcept : kind_(kind) {}

  virtual std::unique_ptr<buffering_visitor> take_child_visits() noexcept {
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }

 private:
  expression_kind kind_;
};

class expression::expression_with_prefix_operator_base : public expression {
 public:
  explicit expression_with_prefix_operator_base(
      expression_kind kind, expression_ptr child,
      source_code_span operator_span) noexcept
      : expression(kind),
        unary_operator_begin_(operator_span.begin()),
        child_(child) {}

  int child_count() const noexcept override { return 1; }

  expression_ptr child(int index) const noexcept override {
    assert(index == 0);
    return this->child_;
  }

  source_code_span span() const noexcept override {
    return source_code_span(this->unary_operator_begin_,
                            this->child_->span().end());
  }

 private:
  const char *unary_operator_begin_;
  expression_ptr child_;
};

class expression::_invalid : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::_invalid;

  explicit _invalid() noexcept : expression(kind) {}

  source_code_span span() const noexcept override {
    assert(false && "Not yet implemented");
    QLJS_UNREACHABLE();
  }
};

class expression::_new : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::_new;

  explicit _new(std::vector<expression_ptr> &&children,
                source_code_span span) noexcept
      : expression(kind), span_(span), children_(std::move(children)) {}

  int child_count() const noexcept override {
    return narrow_cast<int>(this->children_.size());
  }

  expression_ptr child(int index) const noexcept override {
    assert(index >= 0);
    assert(index < static_cast<int>(this->children_.size()));
    return this->children_[index];
  }

  source_code_span span() const noexcept override { return this->span_; }

 private:
  source_code_span span_;
  std::vector<expression_ptr> children_;
};

class expression::_template : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::_template;

  explicit _template(std::vector<expression_ptr> &&children,
                     source_code_span span) noexcept
      : expression(kind), span_(span), children_(std::move(children)) {}

  int child_count() const noexcept override {
    return narrow_cast<int>(this->children_.size());
  }

  expression_ptr child(int index) const noexcept override {
    assert(index >= 0);
    assert(index < static_cast<int>(this->children_.size()));
    return this->children_[index];
  }

  source_code_span span() const noexcept override { return this->span_; }

 private:
  source_code_span span_;
  std::vector<expression_ptr> children_;
};

class expression::array : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::array;

  explicit array(std::vector<expression_ptr> &&children,
                 source_code_span span) noexcept
      : expression(kind), span_(span), children_(std::move(children)) {}

  int child_count() const noexcept override {
    return narrow_cast<int>(this->children_.size());
  }

  expression_ptr child(int index) const noexcept override {
    assert(index >= 0);
    assert(index < static_cast<int>(this->children_.size()));
    return this->children_[index];
  }

  source_code_span span() const noexcept override { return this->span_; }

 private:
  source_code_span span_;
  std::vector<expression_ptr> children_;
};

class expression::arrow_function_with_expression : public expression {
 public:
  static constexpr expression_kind kind =
      expression_kind::arrow_function_with_expression;

  explicit arrow_function_with_expression(
      function_attributes attributes, expression_ptr body,
      const char *parameter_list_begin) noexcept
      : expression(kind),
        parameter_list_begin_(parameter_list_begin),
        function_attributes_(attributes),
        children_{body} {}

  explicit arrow_function_with_expression(
      function_attributes attributes, std::vector<expression_ptr> &&parameters,
      expression_ptr body, const char *parameter_list_begin) noexcept
      : expression(kind),
        parameter_list_begin_(parameter_list_begin),
        function_attributes_(attributes),
        children_(std::move(parameters)) {
    this->children_.emplace_back(body);
  }

  int child_count() const noexcept override {
    return narrow_cast<int>(this->children_.size());
  }

  expression_ptr child(int index) const noexcept override {
    assert(index >= 0);
    assert(index < static_cast<int>(this->children_.size()));
    return this->children_[index];
  }

  source_code_span span() const noexcept override {
    return source_code_span(this->parameter_list_begin_,
                            this->children_.back()->span().end());
  }

  function_attributes attributes() const noexcept override {
    return this->function_attributes_;
  }

 private:
  const char *parameter_list_begin_;
  function_attributes function_attributes_;
  std::vector<expression_ptr> children_;
};

class expression::arrow_function_with_statements : public expression {
 public:
  static constexpr expression_kind kind =
      expression_kind::arrow_function_with_statements;

  explicit arrow_function_with_statements(
      function_attributes attributes,
      std::unique_ptr<buffering_visitor> &&child_visits,
      source_code_span span) noexcept
      : expression(kind),
        function_attributes_(attributes),
        span_(span),
        child_visits_(std::move(child_visits)) {}

  explicit arrow_function_with_statements(
      function_attributes attributes, std::vector<expression_ptr> &&parameters,
      std::unique_ptr<buffering_visitor> &&child_visits,
      source_code_span span) noexcept
      : expression(kind),
        function_attributes_(attributes),
        span_(span),
        child_visits_(std::move(child_visits)),
        children_(std::move(parameters)) {}

  int child_count() const noexcept override {
    return narrow_cast<int>(this->children_.size());
  }

  expression_ptr child(int index) const noexcept override {
    assert(index >= 0);
    assert(index < static_cast<int>(this->children_.size()));
    return this->children_[index];
  }

  source_code_span span() const noexcept override { return this->span_; }

  function_attributes attributes() const noexcept override {
    return this->function_attributes_;
  }

 protected:
  std::unique_ptr<buffering_visitor> take_child_visits() noexcept override {
    return std::move(this->child_visits_);
  }

 private:
  function_attributes function_attributes_;
  source_code_span span_;
  std::unique_ptr<buffering_visitor> child_visits_;
  std::vector<expression_ptr> children_;
};

class expression::assignment : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::assignment;

  explicit assignment(expression_kind kind, expression_ptr lhs,
                      expression_ptr rhs) noexcept
      : expression(kind), children_{lhs, rhs} {
    assert(kind == expression_kind::assignment ||
           kind == expression_kind::compound_assignment);
  }

  int child_count() const noexcept override {
    return narrow_cast<int>(this->children_.size());
  }

  expression_ptr child(int index) const noexcept override {
    assert(index >= 0);
    assert(index < static_cast<int>(this->children_.size()));
    return this->children_[index];
  }

  source_code_span span() const noexcept override {
    return source_code_span(this->children_.front()->span().begin(),
                            this->children_.back()->span().end());
  }

 private:
  std::array<expression_ptr, 2> children_;
};

class expression::await
    : public expression::expression_with_prefix_operator_base {
 public:
  static constexpr expression_kind kind = expression_kind::await;

  explicit await(expression_ptr child, source_code_span operator_span) noexcept
      : expression::expression_with_prefix_operator_base(kind, child,
                                                         operator_span) {}
};

class expression::binary_operator : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::binary_operator;

  explicit binary_operator(std::vector<expression_ptr> &&children) noexcept
      : expression(kind), children_(std::move(children)) {}

  int child_count() const noexcept override {
    return narrow_cast<int>(this->children_.size());
  }

  expression_ptr child(int index) const noexcept override {
    assert(index >= 0);
    assert(index < static_cast<int>(this->children_.size()));
    return this->children_[index];
  }

  source_code_span span() const noexcept override {
    return source_code_span(this->children_.front()->span().begin(),
                            this->children_.back()->span().end());
  }

 private:
  std::vector<expression_ptr> children_;
};

class expression::call : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::call;

  explicit call(std::vector<expression_ptr> &&children,
                source_code_span span) noexcept
      : expression(kind),
        call_right_paren_end_(span.end()),
        children_(std::move(children)) {}

  int child_count() const noexcept override {
    return narrow_cast<int>(this->children_.size());
  }

  expression_ptr child(int index) const noexcept override {
    assert(index >= 0);
    assert(index < static_cast<int>(this->children_.size()));
    return this->children_[index];
  }

  source_code_span span() const noexcept override {
    return source_code_span(this->children_.front()->span().begin(),
                            this->call_right_paren_end_);
  }

 private:
  const char *call_right_paren_end_;
  std::vector<expression_ptr> children_;
};

class expression::conditional : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::conditional;

  explicit conditional(expression_ptr condition, expression_ptr true_branch,
                       expression_ptr false_branch) noexcept
      : expression(kind), children_{condition, true_branch, false_branch} {}

  int child_count() const noexcept override {
    return narrow_cast<int>(this->children_.size());
  }

  expression_ptr child(int index) const noexcept override {
    assert(index >= 0);
    assert(index < static_cast<int>(this->children_.size()));
    return this->children_[index];
  }

  source_code_span span() const noexcept override {
    return source_code_span(this->children_.front()->span().begin(),
                            this->children_.back()->span().end());
  }

 private:
  std::array<expression_ptr, 3> children_;
};

class expression::dot : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::dot;

  explicit dot(expression_ptr lhs, identifier rhs) noexcept
      : expression(kind), variable_identifier_(rhs), child_(lhs) {}

  int child_count() const noexcept override { return 1; }

  expression_ptr child(int index) const noexcept override {
    assert(index == 0);
    return this->child_;
  }

  identifier variable_identifier() const noexcept override {
    return this->variable_identifier_;
  }

  source_code_span span() const noexcept override {
    return source_code_span(this->child_0()->span().begin(),
                            this->variable_identifier_.span().end());
  }

 private:
  identifier variable_identifier_;
  expression_ptr child_;
};

class expression::function : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::function;

  explicit function(function_attributes attributes,
                    std::unique_ptr<buffering_visitor> &&child_visits,
                    source_code_span span) noexcept
      : expression(kind),
        function_attributes_(attributes),
        child_visits_(std::move(child_visits)),
        span_(span) {}

  source_code_span span() const noexcept override { return this->span_; }

  function_attributes attributes() const noexcept override {
    return this->function_attributes_;
  }

 protected:
  std::unique_ptr<buffering_visitor> take_child_visits() noexcept override {
    return std::move(this->child_visits_);
  }

 private:
  function_attributes function_attributes_;
  std::unique_ptr<buffering_visitor> child_visits_;
  source_code_span span_;
};

class expression::import : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::import;

  explicit import(source_code_span span) noexcept
      : expression(kind), span_(span) {}

  source_code_span span() const noexcept override { return this->span_; }

 private:
  source_code_span span_;
};

class expression::index : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::index;

  explicit index(expression_ptr container, expression_ptr subscript,
                 const char *subscript_end) noexcept
      : expression(kind),
        index_subscript_end_(subscript_end),
        children_{container, subscript} {}

  int child_count() const noexcept override {
    return narrow_cast<int>(this->children_.size());
  }

  expression_ptr child(int index) const noexcept override {
    assert(index >= 0);
    assert(index < static_cast<int>(this->children_.size()));
    return this->children_[index];
  }

  source_code_span span() const noexcept override {
    return source_code_span(this->child_0()->span().begin(),
                            this->index_subscript_end_);
  }

 private:
  const char *index_subscript_end_;
  std::array<expression_ptr, 2> children_;
};

class expression::literal : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::literal;

  explicit literal(source_code_span span) noexcept
      : expression(kind), span_(span) {}

  source_code_span span() const noexcept override { return this->span_; }

 private:
  source_code_span span_;
};

class expression::named_function : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::named_function;

  explicit named_function(function_attributes attributes, identifier name,
                          std::unique_ptr<buffering_visitor> &&child_visits,
                          source_code_span span) noexcept
      : expression(kind),
        function_attributes_(attributes),
        child_visits_(std::move(child_visits)),
        variable_identifier_(name),
        span_(span) {}

  identifier variable_identifier() const noexcept override {
    return this->variable_identifier_;
  }

  source_code_span span() const noexcept override { return this->span_; }

  function_attributes attributes() const noexcept override {
    return this->function_attributes_;
  }

 protected:
  std::unique_ptr<buffering_visitor> take_child_visits() noexcept override {
    return std::move(this->child_visits_);
  }

 private:
  function_attributes function_attributes_;
  std::unique_ptr<buffering_visitor> child_visits_;
  identifier variable_identifier_;
  source_code_span span_;
};

class expression::object : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::object;

  explicit object(std::vector<object_property_value_pair> &&entries,
                  source_code_span span) noexcept
      : expression(kind), span_(span), entries_(std::move(entries)) {}

  int object_entry_count() const noexcept override {
    return narrow_cast<int>(this->entries_.size());
  }

  object_property_value_pair object_entry(int index) const noexcept override {
    assert(index >= 0);
    assert(index < this->object_entry_count());
    return this->entries_[index];
  }

  source_code_span span() const noexcept override { return this->span_; }

 private:
  source_code_span span_;
  std::vector<expression::object_property_value_pair> entries_;
};

class expression::rw_unary_prefix
    : public expression::expression_with_prefix_operator_base {
 public:
  static constexpr expression_kind kind = expression_kind::rw_unary_prefix;

  explicit rw_unary_prefix(expression_ptr child,
                           source_code_span operator_span) noexcept
      : expression::expression_with_prefix_operator_base(kind, child,
                                                         operator_span) {}
};

class expression::rw_unary_suffix : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::rw_unary_suffix;

  explicit rw_unary_suffix(expression_ptr child,
                           source_code_span operator_span) noexcept
      : expression(kind),
        unary_operator_end_(operator_span.end()),
        child_(child) {}

  int child_count() const noexcept override { return 1; }

  expression_ptr child(int index) const noexcept override {
    assert(index == 0);
    return this->child_;
  }

  source_code_span span() const noexcept override {
    return source_code_span(this->child_->span().begin(),
                            this->unary_operator_end_);
  }

 private:
  const char *unary_operator_end_;
  expression_ptr child_;
};

class expression::spread
    : public expression::expression_with_prefix_operator_base {
 public:
  static constexpr expression_kind kind = expression_kind::spread;

  explicit spread(expression_ptr child, source_code_span operator_span) noexcept
      : expression::expression_with_prefix_operator_base(kind, child,
                                                         operator_span) {}
};

class expression::super : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::super;

  explicit super(source_code_span span) noexcept
      : expression(kind), span_(span) {}

  source_code_span span() const noexcept override { return this->span_; }

 private:
  source_code_span span_;
};

class expression::unary_operator
    : public expression::expression_with_prefix_operator_base {
 public:
  static constexpr expression_kind kind = expression_kind::unary_operator;

  explicit unary_operator(expression_ptr child,
                          source_code_span operator_span) noexcept
      : expression::expression_with_prefix_operator_base(kind, child,
                                                         operator_span) {}
};

class expression::variable : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::variable;

  explicit variable(identifier variable_identifier) noexcept
      : expression(kind), variable_identifier_(variable_identifier) {}

  identifier variable_identifier() const noexcept override {
    return this->variable_identifier_;
  }

  source_code_span span() const noexcept override {
    return this->variable_identifier_.span();
  }

 private:
  identifier variable_identifier_;
};

class expression_arena {
 public:
  template <class Expression, class... Args>
  expression_ptr make_expression(Args &&... args) {
    std::unique_ptr<Expression> ast =
        std::make_unique<Expression>(std::forward<Args>(args)...);
    this->expressions_.emplace_back(std::move(ast));
    return expression_ptr(this->expressions_.back().get());
  }

 private:
  std::deque<std::unique_ptr<expression>> expressions_;
};
}  // namespace quick_lint_js

#undef QLJS_UNEXPECTED_EXPRESSION_KIND

#endif
